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
import cats.state._
import cats.state.State._
import cats.state.StateT._
import org.joda.time.DateTime

sealed trait Extractor[Row, A] {
  type SingleResult

  val M = MonadState[StateT[Eval, ?, ?], IndexedSeq[Row]]

  private[extractor] def extractRow: StateT[Eval, IndexedSeq[Row], Option[A]]

  def extractHeadOption(rows: Iterable[Row]): Option[SingleResult]
  def extractAll(rows: Iterable[Row]): List[SingleResult]

  def map[B](func: A => B) = MappedExtractor(this, func)
  def map[B](func: A => B, unapplyFunc: B => Option[Any]) = MappedExtractor(this, func, Some(unapplyFunc))
  def asOption = OptionExtractor(this)

  protected def checkNullValueAndGet[T](t: Option[T]) =
    t.getOrElse(throw new NullPointerException("Tried to extract a null value without an OptionExtractor"))
}

/**
 * A SimpleExtractor is an Extractor where the extracted type is the same as the emitted type.
 * This is the case for almost all extractors
 */
trait SimpleExtractor[Row, A] {
  this: Extractor[Row, A] =>

  type SingleResult = A

  def extractHeadOption(iterable: Iterable[Row]): Option[A] = {
    val rows = iterable.toIndexedSeq

    if (rows.isEmpty)
      None
    else {
      val value = extractRow.runA(rows).value
      Some(checkNullValueAndGet(value))
    }
  }

  def extractAll(iterable: Iterable[Row]): List[A] = {
    val rows = iterable.toIndexedSeq

    @annotation.tailrec
    def loop(innerRows: IndexedSeq[Row], results: IndexedSeq[A] = IndexedSeq.empty[A]): List[A] = {
      if (innerRows.isEmpty)
        results.toList
      else {
        val (remaining, next) = extractRow.run(innerRows).value
        loop(remaining, results :+ checkNullValueAndGet(next))
      }
    }

    loop(rows)
  }

  // def groupBy[B](groupBy: Extractor[Row, B]) = GroupedExtractor(this, groupBy)
}

trait SingleRowExtractor[Row, A] {
  this: Extractor[Row, A] =>
  // def asList = ListMultiRowExtractor(this)
}

/**
 * Extractor that always returns the same value
 */
case class ConstantExtractor[Row, A](value: A) extends Extractor[Row, A] with SimpleExtractor[Row, A] with SingleRowExtractor[Row, A] {
  import M._

  private[extractor] def extractRow = for {
    rows <- get
    _ <- set(rows.drop(1))
  } yield Option(value)
}

/**
 * Extractor that emits the values for a single cell.
 */
trait CellExtractor[Row, A] extends Extractor[Row, A] with SimpleExtractor[Row, A] with SingleRowExtractor[Row, A] {
  import M._

  private[extractor] def extractRow = for {
    rows <- get
    result = rows.headOption.flatMap(read)
    _ <- set(rows.drop(1))
  } yield result

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
  import M._

  private[extractor] def extractRow = for {
    rows <- get
    result <- inner.extractRow
  } yield result.map(func)
}

// case class AppedExtractor[Row, A, B](fa: Extractor[Row, A], fab: Extractor[Row, A => B]) extends Extractor[Row, B] with SimpleExtractor[Row, B] with SingleRowExtractor[Row, B] {

// }

// /**
//  * An extractor that aggregates results from a seq of extractors into a seq.
//  */
// case class SeqExtractor[Row, A](extractors: Seq[Extractor[Row, A]]) extends Extractor[Row, Seq[A]] with SimpleExtractor[Row, Seq[A]] with SingleRowExtractor[Row, Seq[A]] {

// }

/**
 * An extractor that returns `None` if all of the cells in the
 * `inner` extractor are `null` in the row.
 *
 * If any underlying cell is non-`null`, this returns `Some`
 * of `inner`'s result.
 */
case class OptionExtractor[Row, A](inner: Extractor[Row, A]) extends Extractor[Row, Option[A]] with SimpleExtractor[Row, Option[A]] with SingleRowExtractor[Row, Option[A]] {
  import M._

  private[extractor] def extractRow = for {
    rows <- get
    result <- inner.extractRow
  } yield Some(result)
}

// /**
//  * An extractor that unwraps an OptionExtractor
//  *
//  * This means that null values can be returned from this extractor and so it later must be wrapped in an OptionExtractor
//  */
// case class NonOptionExtractor[Row, A](inner: Extractor[Row, Option[A]]) extends Extractor[Row, A] with SimpleExtractor[Row, A] with SingleRowExtractor[Row, A] {

// }

// /**
//  * An extractor that accumulates results from rows into a list.
//  */
// case class ListMultiRowExtractor[Row, A](inner: Extractor[Row, A]) extends Extractor[Row, List[A]] with SimpleExtractor[Row, List[A]] {

// }

// /**
//  * An extractor that accumulates results with the same groupBy value into the same value
//  */
// case class GroupedExtractor[Row, A, B](inner: Extractor[Row, A], groupBy: Extractor[Row, B]) extends Extractor[Row, List[A]] {

// }

object Extractor {

  case class Continuation[R, A](run: (A => R) => R)

  implicit def contMonad[R] = new Monad[Continuation[R, ?]] {
    def pure[A](a: A) = Continuation(cb => cb(a))

    def flatMap[A, B](callbackA: Continuation[R, A])(f: A => Continuation[R, B]) =
      Continuation[R, B] { callbackB => callbackA.run { a => f(a).run(callbackB) } }
  }

  implicit class ExtractorOps(extractor: Extractor[_, _]) {
    def findCellExtractor(path: String) = ExtractorFinder(extractor, path)
  }

  //   implicit class OptionExtractorOps[Row, A](optionExtractor: Extractor[Row, Option[A]]) {
  //     def asNonOption = NonOptionExtractor(optionExtractor)
  //   }

  //   implicit def extractorIsApplicative[Row]: Applicative[Extractor[Row, ?]] = new Applicative[Extractor[Row, ?]] {
  //     def pure[A](a: A) = ConstantExtractor(a)
  //     def ap[A, B](fa: Extractor[Row, A])(fab: Extractor[Row, A => B]): Extractor[Row, B] = AppedExtractor(fa, fab)
  //   }
}
