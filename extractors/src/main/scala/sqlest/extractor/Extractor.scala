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

import cats._
import cats.implicits._
import cats.data.Streaming
import org.joda.time.DateTime
import scala.collection.immutable.{ Queue, ListMap }
import scala.language.higherKinds

sealed trait Extractor[Row, A] {
  type Accumulator
  type SingleResult

  def accumulate(rows: Streaming[Row]): Streaming[Accumulator]
  def emit(results: Streaming[Accumulator]): Streaming[Option[A]]

  def run(rows: Streaming[Row]): Streaming[Option[A]] = emit(accumulate(rows))

  def extractHeadOption(rows: Iterable[Row]): Option[SingleResult]
  def extractAll(rows: Iterable[Row]): List[SingleResult]

  protected def checkNullValueAndGet[T](t: Option[T]) =
    t.getOrElse(throw new NullPointerException("Tried to extract a null value without an OptionExtractor"))

  def map[B](func: A => B) = MappedExtractor(this, func)
  def map[B](func: A => B, unapplyFunc: B => Option[Any]) = MappedExtractor(this, func, Some(unapplyFunc))
  def asOption = OptionExtractor(this)
}

/**
 * A SimpleExtractor is an Extractor where the extracted type is the same as the emitted type.
 * This is the case for almost all extractors
 */
trait SimpleExtractor[Row, A] {
  this: Extractor[Row, A] =>

  type SingleResult = A

  def accumulate(rows: Streaming[Row]): Streaming[Accumulator]
  def emit(results: Streaming[Accumulator]): Streaming[Option[SingleResult]]

  def extractHeadOption(rows: Iterable[Row]): Option[A] = {
    val resultStream = Streaming.fromIterable(rows)
    run(resultStream.take(1)).foldLeft(Option.empty[A]) {
      case (_, result) => Some(checkNullValueAndGet(result))
    }
  }

  def extractAll(rows: Iterable[Row]): List[A] = {
    val resultStream = Streaming.fromIterable(rows)
    run(resultStream).foldLeft(Seq.empty[A]) {
      case (acc, next) => acc :+ checkNullValueAndGet(next)
    }.toList
  }

  // def groupBy[B](groupBy: Extractor[Row, B]) = GroupedExtractor(this, groupBy)
}

trait SingleRowExtractor[Row, A] {
  this: Extractor[Row, A] =>
  def asList = ListMultiRowExtractor(this)
}

/**
 * Extractor that always returns the same value
 */
case class ConstantExtractor[Row, A](value: A) extends Extractor[Row, A] with SimpleExtractor[Row, A] with SingleRowExtractor[Row, A] {
  type Accumulator = Option[A]
  def accumulate(rows: Streaming[Row]) = rows.map { _ => Some(value) }
  def emit(results: Streaming[Accumulator]) = results
}

/**
 * Extractor that emits the values for a single cell.
 */
trait CellExtractor[Row, A] extends Extractor[Row, A] with SimpleExtractor[Row, A] with SingleRowExtractor[Row, A] {
  type Accumulator = Option[A]
  def accumulate(rows: Streaming[Row]) = rows.map { row => read(row) }
  def emit(results: Streaming[Accumulator]) = results
  def read(row: Row): Option[A]
}

/**
 * An extractor acts as a base type for extracting Product types
 */
trait ProductExtractor[Row, A <: Product] extends Extractor[Row, A] with SimpleExtractor[Row, A] with SingleRowExtractor[Row, A] {
  def innerExtractors: List[Extractor[Row, _]]
}

/**
 * Add ProductExtractorNames to a ProductExtractor to allow
 * inner extractors to be found by name
 */
trait ProductExtractorNames {
  this: ProductExtractor[_, _] =>
  def innerExtractorNames: List[String]
}

/**
 * An extractor that behaves as `inner` but pipes its `emitted` values through `func`.
 */
case class MappedExtractor[Row, A, B](inner: Extractor[Row, A], func: A => B, unapplyMethod: Option[B => Option[Any]] = None) extends Extractor[Row, B] with SimpleExtractor[Row, B] with SingleRowExtractor[Row, B] {
  type Accumulator = inner.Accumulator
  def accumulate(rows: Streaming[Row]) = inner.accumulate(rows)
  def emit(results: Streaming[inner.Accumulator]) = inner.emit(results).map(Functor[Option].lift(func))
}

/**
 * An extractor that aggregates results from a seq of extractors into a seq.
 */
case class SeqExtractor[Row, A](extractors: Seq[Extractor[Row, A]]) extends Extractor[Row, Seq[A]] with SimpleExtractor[Row, Seq[A]] with SingleRowExtractor[Row, Seq[A]] {
  type Accumulator = Seq[Option[A]]

  def accumulate(rows: Streaming[Row]) = rows.map { row =>
    extractors
      .map { extractor => extractor.run(Streaming(row)) }
      .flatMap { stream => stream.toList }
  }

  def emit(results: Streaming[Accumulator]) =
    results.map { seq => seq.toList.sequence }
}

/**
 * An extractor that returns `None` if all of the cells in the
 * `inner` extractor are `null` in the row.
 *
 * If any underlying cell is non-`null`, this returns `Some`
 * of `inner`'s result.
 */
case class OptionExtractor[Row, A](inner: Extractor[Row, A]) extends Extractor[Row, Option[A]] with SimpleExtractor[Row, Option[A]] with SingleRowExtractor[Row, Option[A]] {
  type Accumulator = inner.Accumulator
  def accumulate(rows: Streaming[Row]) = inner.accumulate(rows)
  def emit(results: Streaming[Accumulator]) = inner.emit(results).map(Some(_))
}

/**
 * An extractor that unwraps an OptionExtractor
 *
 * This means that null values can be returned from this extractor and so it later must be wrapped in an OptionExtractor
 */
case class NonOptionExtractor[Row, A](inner: Extractor[Row, Option[A]]) extends Extractor[Row, A] with SimpleExtractor[Row, A] with SingleRowExtractor[Row, A] {
  type Accumulator = inner.Accumulator
  def accumulate(rows: Streaming[Row]) = inner.accumulate(rows)
  def emit(results: Streaming[Accumulator]) = inner.emit(results).map(_.get)
}

/**
 * An extractor that accumulates results from rows into a list.
 */
case class ListMultiRowExtractor[Row, A](inner: Extractor[Row, A]) extends Extractor[Row, List[A]] with SimpleExtractor[Row, List[A]] {
  type Accumulator = List[Option[A]]

  def accumulate(rows: Streaming[Row]) = Streaming(inner.run(rows).toList)

  def emit(results: Streaming[Accumulator]) = results.flatMap { list =>
    val noRowsEmpty = list.forall(!_.isEmpty)
    val allRowsEmpty = list.forall(_.isEmpty)

    if (list.isEmpty) Streaming.empty[Option[List[A]]]
    else if (noRowsEmpty) Streaming(Some(list.map(_.get)))
    else if (allRowsEmpty) Streaming(Some(Nil))
    else Streaming(None)
  }

  // In a left join either all row are full or all rows are null.
  // These are the valid accumulators that will return a list
  // def emit(accumulator: Queue[Option[A]]) = {
  //   val noRowsEmpty = accumulator.forall(!_.isEmpty)
  //   val allRowsEmpty = accumulator.forall(_.isEmpty)
  //   if (noRowsEmpty)
  //     Some(accumulator.map(_.get).toList)
  //   else if (allRowsEmpty)
  //     Some(Nil)
  //   else
  //     None
  // }
}

// /**
//  * An extractor that accumulates results with the same groupBy value into the same value
//  */
// case class GroupedExtractor[Row, A, B](inner: Extractor[Row, A], groupBy: Extractor[Row, B]) extends Extractor[Row, List[A]] {
//   // Consider using a tuple of a Queue and a HashMap as the Accumulator for efficiency
//   type SingleResult = A
//   type Accumulator = ListMap[B, inner.Accumulator]

//   def initialize(row: Row) = {
//     val groupByKey = groupBy.emit(groupBy.initialize(row))
//     val accumulator =
//       for { key <- groupByKey }
//         yield ListMap(key -> inner.initialize(row))

//     accumulator getOrElse ListMap()
//   }

//   def accumulate(accumulator: ListMap[B, inner.Accumulator], row: Row) = {
//     val groupByKey = groupBy.emit(groupBy.initialize(row))

//     val newInnerAccumulator = for { key <- groupByKey }
//       yield accumulator.get(key) match {
//       case Some(innerAccumulator) => inner.accumulate(innerAccumulator, row)
//       case None => inner.initialize(row)
//     }

//     val newOuterAccumulator = for {
//       key <- groupByKey
//       acc <- newInnerAccumulator
//     } yield accumulator + (key -> acc)

//     newOuterAccumulator getOrElse accumulator
//   }

//   def emit(accumulator: ListMap[B, inner.Accumulator]) = Some(accumulator.values.map(inner.emit).toList.map(checkNullValueAndGet))

//   def extractHeadOption(rows: Iterable[Row]): Option[A] = {
//     val rowIterator = rows.iterator
//     if (rowIterator.hasNext) {
//       var accumulator = initialize(rowIterator.next)

//       while (rowIterator.hasNext && accumulator.size == 1)
//         accumulator = accumulate(accumulator, rowIterator.next)

//       checkNullValueAndGet(emit(accumulator)).headOption
//     } else None
//   }

//   def extractAll(rows: Iterable[Row]): List[A] = {
//     val rowIterator = rows.iterator
//     if (rowIterator.hasNext) {
//       var accumulator = initialize(rowIterator.next)

//       while (rowIterator.hasNext)
//         accumulator = accumulate(accumulator, rowIterator.next)

//       checkNullValueAndGet(emit(accumulator))
//     } else Nil
//   }
// }

// object Extractor {
//   implicit class ExtractorOps(extractor: Extractor[_, _]) {
//     def findCellExtractor(path: String) = ExtractorFinder(extractor, path)
//   }

//   implicit class OptionExtractorOps[Row, A](optionExtractor: Extractor[Row, Option[A]]) {
//     def asNonOption = NonOptionExtractor(optionExtractor)
//   }

//   implicit def extractorIsApplicative[Row]: Applicative[Extractor[Row, ?]] = new Applicative[Extractor[Row, ?]] {
//     def pure[A](a: A) = ConstantExtractor(a)
//     def ap[A, B](fa: Extractor[Row, A])(fab: Extractor[Row, A => B]): Extractor[Row, B] = {
//       fa
//     }
//   }
// }
