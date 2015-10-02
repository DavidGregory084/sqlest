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

package sqlest.ast
package syntax

import sqlest.ast._

trait ColumnSyntax {
  implicit def literalColumn[A](value: A)(implicit columnType: ColumnType[A]): Column[A] =
    LiteralColumn[A](value)

  implicit def literalColumn[A](value: Some[A])(implicit columnType: ColumnType[Option[A]]): Column[Option[A]] =
    LiteralColumn[Option[A]](value)

  implicit class LiteralColumnOps[A](left: A) {
    def column[B >: A](implicit columnType: ColumnType[B]) = LiteralColumn[B](left)
  }

  implicit class SomeLiteralColumnOps[A](left: Some[A]) {
    def column[B >: A](implicit columnType: ColumnType[Option[B]]) = LiteralColumn[Option[B]](left)
  }

  /**
   * This enrichment allows writing 1.constant or "abc".constant, which will directly embed
   * the constant value into the generated sql statement. Do not use this on user input as
   * you will enable SQL injection attacks
   */
  implicit class ConstantColumnOps[A](value: A) {
    def constant[B >: A](implicit columnType: ColumnType[B]) = ConstantColumn[B](value)
  }

  implicit class SomeConstantColumnOps[A](value: Some[A]) {
    def constant[B >: A](implicit columnType: ColumnType[Option[B]]) = ConstantColumn[Option[B]](value)
  }

  /**
   * This implicit allows the use of `TableColumn -> Column` in setters
   */
  implicit def columnSetterPair[A, B](pair: (TableColumn[A], Column[B]))(implicit equivalence: ColumnTypeEquivalence[A, B]) =
    Setter[A, B](pair._1, pair._2)

  /**
   * This implicit allows the use of `TableColumn -> Value` in setters,
   * as opposed to `TableColumn -> Column` as is actually required:
   */
  implicit def literalSetterPair[A, B](pair: (TableColumn[A], B))(implicit valueType: ColumnType[B], equivalence: ColumnTypeEquivalence[A, B]) =
    Setter[A, B](pair._1, pair._2.column)

  implicit class AliasColumnOps[A](left: Column[A]) {
    def as(alias: String) = left match {
      case AliasColumn(column, _) => AliasColumn[A](column, alias)(left.columnType)
      case _ => AliasColumn[A](left, alias)(left.columnType)
    }

    def as(tableAlias: String, alias: String) = left match {
      case AliasColumn(column, _) => AliasColumn[A](column, tableAlias + "_" + alias)(left.columnType)
      case _ => AliasColumn[A](left, tableAlias + "_" + alias)(left.columnType)
    }
  }

  /**
   * This implicit conversion allows using as a column: a select statement which selects a single column
   */
  implicit def SelectColumnOps[A](select: Select[AliasedColumn[A], _ <: Relation]) =
    SelectColumn(select)(select.cols.columnType)

  implicit class NullableColumnsOps[A](column: Column[A]) {
    def isNull = {
      val columnIsNull = PostfixFunctionColumn[Boolean]("is null", column)

      column.columnType match {
        case optionColumnType: OptionColumnType[A, _] if !optionColumnType.hasNullNullValue =>
          columnIsNull || InfixFunctionColumn[Boolean]("=", column, ConstantColumn[A](None.asInstanceOf[A])(optionColumnType))
        case _ =>
          columnIsNull
      }
    }

    def isNotNull = {
      val columnIsNotNull = PostfixFunctionColumn[Boolean]("is not null", column)

      column.columnType match {
        case optionColumnType: OptionColumnType[A, _] if !optionColumnType.hasNullNullValue =>
          columnIsNotNull && InfixFunctionColumn[Boolean]("<>", column, ConstantColumn[A](None.asInstanceOf[A])(optionColumnType))
        case _ =>
          columnIsNotNull
      }
    }
  }

  implicit class AliasedOptionColumnsOps[A](left: AliasedColumn[A]) {
    def ? = left match {
      case column: TableColumn[_] => AliasColumn(column, left.columnAlias)(left.columnType.toOptionColumnType)
      case AliasColumn(column, columnAlias) => AliasColumn(column, columnAlias)(left.columnType.toOptionColumnType)
      case column: ReferenceColumn[A] => ReferenceColumn(left.columnAlias)(left.columnType.toOptionColumnType)
    }
  }

  implicit class ComparisonColumnOps[A](left: Column[A]) {
    implicit val leftType: ColumnType[A] = left.columnType

    def ===[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val (mappedLeft, mappedRight) = ColumnTypeEquivalence.alignColumnTypes(left, right)
      InfixFunctionColumn[Boolean]("=", mappedLeft, mappedRight)
    }

    def =!=[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val (mappedLeft, mappedRight) = ColumnTypeEquivalence.alignColumnTypes(left, right)
      InfixFunctionColumn[Boolean]("<>", mappedLeft, mappedRight)
    }

    def >[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val (mappedLeft, mappedRight) = ColumnTypeEquivalence.alignColumnTypes(left, right)
      InfixFunctionColumn[Boolean](">", mappedLeft, mappedRight)
    }

    def <[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val (mappedLeft, mappedRight) = ColumnTypeEquivalence.alignColumnTypes(left, right)
      InfixFunctionColumn[Boolean]("<", mappedLeft, mappedRight)
    }

    def >=[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val (mappedLeft, mappedRight) = ColumnTypeEquivalence.alignColumnTypes(left, right)
      InfixFunctionColumn[Boolean](">=", mappedLeft, mappedRight)
    }

    def <=[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val (mappedLeft, mappedRight) = ColumnTypeEquivalence.alignColumnTypes(left, right)
      InfixFunctionColumn[Boolean]("<=", mappedLeft, mappedRight)
    }

    def between[B, C](lower: Column[B], upper: Column[C])(implicit lowerEquivalence: ColumnTypeEquivalence[A, B], upperEquivalence: ColumnTypeEquivalence[A, C]) = {
      val (mappedLeftLower, mappedLower) = ColumnTypeEquivalence.alignColumnTypes(left, lower)(lowerEquivalence)
      val (mappedLeftUpper, mappedUpper) = ColumnTypeEquivalence.alignColumnTypes(left, upper)(upperEquivalence)
      if (mappedLeftLower.columnType == mappedLeftUpper.columnType)
        DoubleInfixFunctionColumn[Boolean]("between", "and", left, mappedLower, mappedUpper)
      else
        throw new AssertionError("Cannot use between with different MappedColumns for lower and upper")
    }

    def in[B](values: Column[B]*)(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val mappedValues = values.map(value => ColumnTypeEquivalence.alignColumnTypes(left, value)._2)
      InfixFunctionColumn[Boolean]("in", left, ScalarFunctionColumn("", mappedValues))
    }

    def in[B](values: Seq[B])(implicit rightType: ColumnType[B], equivalence: ColumnTypeEquivalence[A, B]): Column[Boolean] =
      in(values.map(_.column): _*)
  }

  implicit class BooleanColumnOps[A](left: Column[A])(implicit equivalence: ColumnTypeEquivalence[Boolean, A]) {
    def unary_! = PrefixFunctionColumn[Boolean]("not", left)
    def &&[B](right: Column[B])(implicit equivalenceB: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Boolean]("and", left, right)
    def ||[B](right: Column[B])(implicit equivalenceB: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Boolean]("or", left, right)
  }

  implicit class IntColumnOps[A: ColumnType](left: Column[A])(implicit equivalence: ColumnTypeEquivalence[Int, A]) {
    def +[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Int]("+", left, right)
    def -[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Int]("-", left, right)
    def *[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Int]("*", left, right)
    def /[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Double]("/", left, right)
  }

  implicit class StringColumnOps[A](left: Column[A])(implicit equivalence: ColumnTypeEquivalence[String, A]) {
    def ++[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[String]("||", left, right)
    def like[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Boolean]("like", left, right)
  }

}
