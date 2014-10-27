/*
 * Copyright 2014 JHC Systems Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sqlest.extractor

import java.sql.ResultSet
import org.joda.time.DateTime
import scala.collection.immutable._
import sqlest.ast._

sealed trait Extractor[A] {
  type Accumulator
  def columns: List[AliasedColumn[_]]

  type SingleResult

  def extractHeadOption(row: ResultSet): Option[SingleResult]
  def extractAll(row: ResultSet): List[SingleResult]

  def initialize(row: ResultSet): Accumulator
  def accumulate(row: ResultSet, accumulator: Accumulator): Accumulator
  // An Option is emitted to represent null values
  // If a null value is not handled by an OptionExtractor then an exception will be thrown
  def emit(accumulator: Accumulator): Option[A]

  protected def checkNullValueAndGet[T](t: Option[T]) =
    t.getOrElse(throw new NullPointerException("Tried to extract a null value without an OptionExtractor"))

  def map[B](func: A => B) = MappedExtractor(this, func)
  def asOption = OptionExtractor(this)
}

trait SingleExtractor[A] extends Extractor[A] {
  final type SingleResult = A

  final def extractHeadOption(row: ResultSet): Option[A] =
    if (row.isFirst || row.isBeforeFirst && row.next) {
      Some(checkNullValueAndGet(emit(initialize(row))))
    } else None

  final def extractAll(row: ResultSet): List[A] =
    if (row.isFirst || row.isBeforeFirst && row.next) {
      var accumulator = Queue(checkNullValueAndGet(emit(initialize(row))))

      while (row.next)
        accumulator = accumulator :+ checkNullValueAndGet(emit(initialize(row)))

      accumulator.toList
    } else Nil

  def asList = ListExtractor(this)
  def groupBy[B](groupBy: Extractor[B]) = GroupedExtractor(this, groupBy)
}

trait MultiExtractor[A] extends Extractor[List[A]] {
  type Accumulator <: Traversable[_]

  final type SingleResult = A

  final def extractHeadOption(row: ResultSet): Option[A] =
    if (row.isFirst || row.isBeforeFirst && row.next) {
      var accumulator = initialize(row)

      while (row.next && accumulator.size == 1)
        accumulator = accumulate(row, accumulator)

      checkNullValueAndGet(emit(accumulator)).headOption
    } else None

  final def extractAll(row: ResultSet): List[A] =
    if (row.isFirst || row.isBeforeFirst && row.next) {
      var accumulator = initialize(row)

      while (row.next)
        accumulator = accumulate(row, accumulator)

      checkNullValueAndGet(emit(accumulator))
    } else Nil
}

/**
 * Extractor that always returns the same value
 */
case class ConstantExtractor[A](value: A) extends SingleExtractor[A] {
  type Accumulator = A
  val columns = Nil

  def initialize(row: ResultSet) = value
  def accumulate(row: ResultSet, accumulator: A) = value
  def emit(accumulator: A) = Some(accumulator)
}

/**
 * Extractor that emits the values for a single `column`.
 */
case class ColumnExtractor[A](column: AliasedColumn[A]) extends SingleExtractor[A] {
  type Accumulator = Option[A]
  val columns = List(column)
  column.columnType match {
    case _: OptionColumnType[_] => Nil
    case _ => List(column)
  }

  def initialize(row: ResultSet) = read(row, column.columnType)

  def accumulate(row: ResultSet, accumulator: Accumulator) = read(row, column.columnType)

  def emit(accumulator: Accumulator) = accumulator

  private def read[B](row: ResultSet, columnType: ColumnType[B]): Option[B] =
    columnType match {
      case BooleanColumnType => wrapNullableValue(row getBoolean column.columnAlias, row)
      case IntColumnType => wrapNullableValue(row getInt column.columnAlias, row)
      case LongColumnType => wrapNullableValue(row getLong column.columnAlias, row)
      case DoubleColumnType => wrapNullableValue(row getDouble column.columnAlias, row)
      case BigDecimalColumnType => wrapNullableValue(row getBigDecimal column.columnAlias, row).map(BigDecimal.apply)
      case StringColumnType => wrapNullableValue(row getString column.columnAlias, row)
      case DateTimeColumnType => wrapNullableValue(new DateTime(row getDate column.columnAlias), row)
      case OptionColumnType(base) => Some(read(row, base))
      case mapped: MappedColumnType[B, _] => read(row, mapped.baseType).map(mapped.read)
    }

  private def wrapNullableValue[B](value: B, row: ResultSet) =
    if (!row.wasNull) Some(value)
    else None
}

trait ProductExtractor[A <: Product] extends SingleExtractor[A] {
  def innerExtractors: List[Extractor[_]]
}

/**
 * An extractor that behaves as `inner` but pipes its `emitted` values through `func`.
 */
case class MappedExtractor[A, B](inner: Extractor[A], func: A => B) extends SingleExtractor[B] {
  type Accumulator = inner.Accumulator
  val columns = inner.columns

  def initialize(row: ResultSet) = inner.initialize(row)

  def accumulate(row: ResultSet, accumulator: inner.Accumulator) = inner.accumulate(row, accumulator)

  def emit(accumulator: inner.Accumulator) = inner.emit(accumulator).map(func)
}

/**
 * An extractor that returns `None` if all of the columns in the
 * `inner` extractor are `null` in the ResultSet.
 *
 * If any underlying column is non-`null`, this returns `Some`
 * of `inner`'s result.
 */
case class OptionExtractor[A](inner: Extractor[A]) extends SingleExtractor[Option[A]] {
  type Accumulator = inner.Accumulator
  val columns = inner.columns

  def initialize(row: ResultSet) = inner.initialize(row)

  def accumulate(row: ResultSet, accumulator: inner.Accumulator) = inner.accumulate(row, accumulator)

  def emit(accumulator: inner.Accumulator) = Some(inner.emit(accumulator))
}

/**
 * An extractor that accumulates results into a list.
 */
case class ListExtractor[A](inner: Extractor[A]) extends MultiExtractor[A] {
  type Accumulator = Queue[Option[A]]
  val columns = inner.columns

  def initialize(row: ResultSet) = Queue(inner.emit(inner.initialize(row)))

  def accumulate(row: ResultSet, accumulator: Queue[Option[A]]) = accumulator :+ inner.emit(inner.initialize(row))

  // In a left join either all row are full or all rows are null.
  // These are the valid accumulators that will return a list
  def emit(accumulator: Queue[Option[A]]) = {
    val noRowsEmpty = accumulator.forall(!_.isEmpty)
    val allRowsEmpty = accumulator.forall(_.isEmpty)
    if (noRowsEmpty)
      Some(accumulator.map(_.get).toList)
    else if (allRowsEmpty)
      Some(Nil)
    else
      None
  }
}

/**
 * An extractor that accumulates results with the same groupBy value into the same value
 */
case class GroupedExtractor[A, B](inner: Extractor[A], groupBy: Extractor[B]) extends MultiExtractor[A] {
  // Consider using a tuple of a Queue and a HashMap as the Accumulator for efficiency
  type Accumulator = ListMap[B, inner.Accumulator]
  val columns = (inner.columns ++ groupBy.columns).distinct

  def initialize(row: ResultSet) =
    ListMap(checkNullValueAndGet(groupBy.emit(groupBy.initialize(row))) -> inner.initialize(row))

  def accumulate(row: ResultSet, accumulator: ListMap[B, inner.Accumulator]) = {
    val groupByKey = checkNullValueAndGet(groupBy.emit(groupBy.initialize(row)))

    val newInnerAccumulator = accumulator.get(groupByKey) match {
      case Some(innerAccumulator) => inner.accumulate(row, innerAccumulator)
      case None => inner.initialize(row)
    }

    accumulator + (groupByKey -> newInnerAccumulator)
  }

  def emit(accumulator: ListMap[B, inner.Accumulator]) = Some(accumulator.values.map(inner.emit).toList.map(checkNullValueAndGet))
}
