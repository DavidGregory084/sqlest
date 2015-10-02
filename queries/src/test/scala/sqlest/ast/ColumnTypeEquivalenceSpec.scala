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

import org.scalatest._
import org.scalatest.matchers._
import shapeless.test.illTyped
import sqlest.ast._
import sqlest.ast.syntax.all._

class ColumnTypeEquivalenceSpec extends FlatSpec with Matchers {
  case class WrappedString(string: String)

  object TestTable extends Table("test", None) {
    // NonNumericColumnType
    val stringColumn = column[String]("stringColumn")
    val booleanColumn = column[Boolean]("booleanColumn")

    // NumericColumnType
    val intColumn = column[Int]("intColumn")
    val bigDecimalColumn = column[BigDecimal]("bigDecimalColumn")

    // MappedColumnType
    val mappedYNBooleanColumn = column[Boolean]("mappedYNBooleanColumn")(BooleanYNColumnType)
    val mapped10BooleanColumn = column[Boolean]("mapped10BooleanColumn")(Boolean10ColumnType)
    val wrappedStringColumn = column[WrappedString]("wrappedString")

    // OptionColumns
    // .. NonNumericColumnType
    val stringOptionColumn = column[Option[String]]("stringColumn")
    val booleanOptionColumn = column[Option[Boolean]]("booleanColumn")

    // .. NumericColumnType
    val intOptionColumn = column[Option[Int]]("intColumn")
    val bigDecimalOptionColumn = column[Option[BigDecimal]]("bigDecimalColumn")

    // .. MappedColumnType
    val mappedYNBooleanOptionColumn = column[Option[Boolean]]("mappedYNBooleanColumn")(OptionColumnType(BooleanYNColumnType))
    val mapped10BooleanOptionColumn = column[Option[Boolean]]("mapped10BooleanColumn")(OptionColumnType(Boolean10ColumnType))
    val wrappedStringOptionColumn = column[Option[WrappedString]]("wrappedString")
  }

  import TestTable._

  "ColumnTypeEquivalence" should "allow the same NonNumericColumnTypes to be compared" in {
    stringColumn === "hello"
    booleanColumn === true

    stringColumn === stringColumn
    booleanColumn === booleanColumn

    illTyped("stringColumn === true")
    illTyped("booleanColumn === 10")
  }

  it should "allow any NumericColumnType to be compared to any other" in {
    intColumn === 10L
    intColumn === 0.1
    intColumn === BigDecimal("0.1")
    bigDecimalColumn === 10L
    bigDecimalColumn === 0.1
    bigDecimalColumn === BigDecimal("0.1")

    intColumn === bigDecimalColumn

    illTyped("intColumn === true")
    illTyped("bigDecimalColumn === \"some string\"")
  }

  it should "allow MappedColumnTypes to be compared to those with same type and database type" in {
    mappedYNBooleanColumn === true
    mapped10BooleanColumn === true
    wrappedStringColumn === WrappedString("wrapping paper")

    mappedYNBooleanColumn === mappedYNBooleanColumn
    mapped10BooleanColumn === mapped10BooleanColumn
    wrappedStringColumn === wrappedStringColumn

    illTyped("mappedYNBooleanColumn === \"Y\"")
    illTyped("mapped10BooleanColumn === 1")
    illTyped("wrappedString === \"wrapped string\"")
  }

  it should "allow non-Option and Option types to be compared" in {
    stringColumn === Some("hello")
    booleanColumn === Some(true)
    intColumn === Some(10L)
    intColumn === Some(0.1)
    intColumn === Some(BigDecimal("0.1"))
    bigDecimalColumn === Some(10L)
    bigDecimalColumn === Some(0.1)
    bigDecimalColumn === Some(BigDecimal("0.1"))
    mappedYNBooleanColumn === Some(false)
    mapped10BooleanColumn === Some(true)
    wrappedStringColumn === Some(WrappedString("wrapping paper"))

    stringColumn === stringOptionColumn
    booleanColumn === booleanOptionColumn
    intColumn === intOptionColumn
    intColumn === intOptionColumn
    intColumn === intOptionColumn
    bigDecimalColumn === bigDecimalOptionColumn
    bigDecimalColumn === bigDecimalOptionColumn
    bigDecimalColumn === bigDecimalOptionColumn
    mappedYNBooleanColumn === mappedYNBooleanOptionColumn
    mapped10BooleanColumn === mapped10BooleanOptionColumn
    wrappedStringColumn === wrappedStringOptionColumn

    illTyped("stringColumn === Some(true)")
    illTyped("booleanColumn === Some(10)")
    illTyped("intColumn === Some(true)")
    illTyped("mappedYNBooleanColumn === Some(\"Y\")")
    illTyped("mapped10BooleanColumn === Some(1)")
    illTyped("wrappedString === Some(\"wrapped string\")")
  }

  it should "allow Option and non-Option types to be compared" in {
    stringOptionColumn === "hello"
    booleanOptionColumn === true
    intOptionColumn === 10L
    intOptionColumn === 0.1
    intOptionColumn === BigDecimal("0.1")
    bigDecimalOptionColumn === 10L
    bigDecimalOptionColumn === 0.1
    bigDecimalOptionColumn === BigDecimal("0.1")
    mappedYNBooleanOptionColumn === false
    mapped10BooleanOptionColumn === true
    wrappedStringOptionColumn === WrappedString("wrapping paper")

    stringOptionColumn === stringColumn
    booleanOptionColumn === booleanColumn
    intOptionColumn === intColumn
    intOptionColumn === intColumn
    intOptionColumn === intColumn
    bigDecimalOptionColumn === bigDecimalColumn
    bigDecimalOptionColumn === bigDecimalColumn
    bigDecimalOptionColumn === bigDecimalColumn
    mappedYNBooleanOptionColumn === mappedYNBooleanColumn
    mapped10BooleanOptionColumn === mapped10BooleanColumn
    wrappedStringOptionColumn === wrappedStringColumn

    illTyped("stringOptionColumn === true")
    illTyped("booleanOptionColumn === 10")
    illTyped("intOptionColumn === true")
    illTyped("mappedYNBooleanOptionColumn === \"Y\"")
    illTyped("mapped10BooleanOptionColumn === 1")
    illTyped("wrappedOptionString === \"wrapped string\"")
  }

  it should "allow Option and Option types to be compared" in {
    stringOptionColumn === Some("hello")
    booleanOptionColumn === Some(true)
    intOptionColumn === Some(10L)
    intOptionColumn === Some(0.1)
    intOptionColumn === Some(BigDecimal("0.1"))
    bigDecimalOptionColumn === Some(10L)
    bigDecimalOptionColumn === Some(0.1)
    bigDecimalOptionColumn === Some(BigDecimal("0.1"))
    mappedYNBooleanOptionColumn === Some(false)
    mapped10BooleanOptionColumn === Some(true)
    wrappedStringOptionColumn === Some(WrappedString("wrapping paper"))

    stringOptionColumn === stringOptionColumn
    booleanOptionColumn === booleanOptionColumn
    intOptionColumn === intOptionColumn
    intOptionColumn === intOptionColumn
    intOptionColumn === intOptionColumn
    bigDecimalOptionColumn === bigDecimalOptionColumn
    bigDecimalOptionColumn === bigDecimalOptionColumn
    bigDecimalOptionColumn === bigDecimalOptionColumn
    mappedYNBooleanOptionColumn === mappedYNBooleanOptionColumn
    mapped10BooleanOptionColumn === mapped10BooleanOptionColumn
    wrappedStringOptionColumn === wrappedStringOptionColumn

    illTyped("stringOptionColumn === Some(true)")
    illTyped("booleanOptionColumn === Some(10)")
    illTyped("intOptionColumn === Some(true)")
    illTyped("mappedYNBooleanOptionColumn === Some(\"Y\")")
    illTyped("mapped10BooleanOptionColumn === Some(1)")
    illTyped("wrappedOptionString === Some(\"wrapped string\")")
  }

  "ColumnTypeEquivalence.alignColumnTypes" should "set the column type on literals to the equivalent ColumnType" in {
    ColumnTypeEquivalence.alignColumnTypes(mappedYNBooleanColumn, false)._2.columnType should be(BooleanYNColumnType)
    ColumnTypeEquivalence.alignColumnTypes(mapped10BooleanColumn, true)._2.columnType should be(Boolean10ColumnType)
    ColumnTypeEquivalence.alignColumnTypes(wrappedStringColumn, WrappedString("wrapped"))._2.columnType should be(implicitly[ColumnType[WrappedString]])
  }

  it should "wrap a non-Option literal value which is equivalent to an Option" in {
    ColumnTypeEquivalence.alignColumnTypes(stringOptionColumn, "hello")._2.asInstanceOf[LiteralColumn[_]].value should be(Some("hello"))
    ColumnTypeEquivalence.alignColumnTypes(booleanOptionColumn, true)._2.asInstanceOf[LiteralColumn[_]].value should be(Some(true))
    ColumnTypeEquivalence.alignColumnTypes(intOptionColumn, 10L)._2.asInstanceOf[LiteralColumn[_]].value should be(Some(10L))
    ColumnTypeEquivalence.alignColumnTypes(intOptionColumn, 0.1)._2.asInstanceOf[LiteralColumn[_]].value should be(Some(0.1))
    ColumnTypeEquivalence.alignColumnTypes(intOptionColumn, BigDecimal("0.1"))._2.asInstanceOf[LiteralColumn[_]].value should be(Some(BigDecimal("0.1")))
    ColumnTypeEquivalence.alignColumnTypes(bigDecimalOptionColumn, 10L)._2.asInstanceOf[LiteralColumn[_]].value should be(Some(10L))
    ColumnTypeEquivalence.alignColumnTypes(bigDecimalOptionColumn, 0.1)._2.asInstanceOf[LiteralColumn[_]].value should be(Some(0.1))
    ColumnTypeEquivalence.alignColumnTypes(bigDecimalOptionColumn, BigDecimal("0.1"))._2.asInstanceOf[LiteralColumn[_]].value should be(Some(BigDecimal("0.1")))
    ColumnTypeEquivalence.alignColumnTypes(mappedYNBooleanOptionColumn, false)._2.asInstanceOf[LiteralColumn[_]].value should be(Some(false))
    ColumnTypeEquivalence.alignColumnTypes(mapped10BooleanOptionColumn, true)._2.asInstanceOf[LiteralColumn[_]].value should be(Some(true))
    ColumnTypeEquivalence.alignColumnTypes(wrappedStringOptionColumn, WrappedString("wrapping paper"))._2.asInstanceOf[LiteralColumn[_]].value should be(Some(WrappedString("wrapping paper")))

    ColumnTypeEquivalence.alignColumnTypes(stringOptionColumn, "hello".constant)._2.asInstanceOf[ConstantColumn[_]].value should be(Some("hello"))
    ColumnTypeEquivalence.alignColumnTypes(booleanOptionColumn, true.constant)._2.asInstanceOf[ConstantColumn[_]].value should be(Some(true))
    ColumnTypeEquivalence.alignColumnTypes(intOptionColumn, 10L.constant)._2.asInstanceOf[ConstantColumn[_]].value should be(Some(10L))
    ColumnTypeEquivalence.alignColumnTypes(intOptionColumn, 0.1.constant)._2.asInstanceOf[ConstantColumn[_]].value should be(Some(0.1))
    ColumnTypeEquivalence.alignColumnTypes(intOptionColumn, BigDecimal("0.1").constant)._2.asInstanceOf[ConstantColumn[_]].value should be(Some(BigDecimal("0.1")))
    ColumnTypeEquivalence.alignColumnTypes(bigDecimalOptionColumn, 10L.constant)._2.asInstanceOf[ConstantColumn[_]].value should be(Some(10L))
    ColumnTypeEquivalence.alignColumnTypes(bigDecimalOptionColumn, 0.1.constant)._2.asInstanceOf[ConstantColumn[_]].value should be(Some(0.1))
    ColumnTypeEquivalence.alignColumnTypes(bigDecimalOptionColumn, BigDecimal("0.1").constant)._2.asInstanceOf[ConstantColumn[_]].value should be(Some(BigDecimal("0.1")))
    ColumnTypeEquivalence.alignColumnTypes(mappedYNBooleanOptionColumn, false.constant)._2.asInstanceOf[ConstantColumn[_]].value should be(Some(false))
    ColumnTypeEquivalence.alignColumnTypes(mapped10BooleanOptionColumn, true.constant)._2.asInstanceOf[ConstantColumn[_]].value should be(Some(true))
    ColumnTypeEquivalence.alignColumnTypes(wrappedStringOptionColumn, WrappedString("wrapping paper").constant)._2.asInstanceOf[ConstantColumn[_]].value should be(Some(WrappedString("wrapping paper")))
  }

  it should "unwrap a Some literal value which is equivalent to an non-Option" in {
    ColumnTypeEquivalence.alignColumnTypes(stringColumn, Some("hello"))._2.asInstanceOf[LiteralColumn[_]].value should be("hello")
    ColumnTypeEquivalence.alignColumnTypes(booleanColumn, Some(true))._2.asInstanceOf[LiteralColumn[Boolean]].value should be(true)
    ColumnTypeEquivalence.alignColumnTypes(intColumn, Some(10L))._2.asInstanceOf[LiteralColumn[_]].value should be(10L)
    ColumnTypeEquivalence.alignColumnTypes(intColumn, Some(0.1))._2.asInstanceOf[LiteralColumn[_]].value should be(0.1)
    ColumnTypeEquivalence.alignColumnTypes(intColumn, Some(BigDecimal("0.1")))._2.asInstanceOf[LiteralColumn[_]].value should be(BigDecimal("0.1"))
    ColumnTypeEquivalence.alignColumnTypes(bigDecimalColumn, Some(10L))._2.asInstanceOf[LiteralColumn[_]].value should be(10L)
    ColumnTypeEquivalence.alignColumnTypes(bigDecimalColumn, Some(0.1))._2.asInstanceOf[LiteralColumn[_]].value should be(0.1)
    ColumnTypeEquivalence.alignColumnTypes(bigDecimalColumn, Some(BigDecimal("0.1")))._2.asInstanceOf[LiteralColumn[_]].value should be(BigDecimal("0.1"))
    ColumnTypeEquivalence.alignColumnTypes(mappedYNBooleanColumn, Some(false))._2.asInstanceOf[LiteralColumn[Boolean]].value should be(false)
    ColumnTypeEquivalence.alignColumnTypes(mapped10BooleanColumn, Some(true))._2.asInstanceOf[LiteralColumn[Boolean]].value should be(true)
    ColumnTypeEquivalence.alignColumnTypes(wrappedStringColumn, Some(WrappedString("wrapping paper")))._2.asInstanceOf[LiteralColumn[_]].value should be(WrappedString("wrapping paper"))

    ColumnTypeEquivalence.alignColumnTypes(stringColumn, Some("hello").constant)._2.asInstanceOf[ConstantColumn[_]].value should be("hello")
    ColumnTypeEquivalence.alignColumnTypes(booleanColumn, Some(true).constant)._2.asInstanceOf[ConstantColumn[Boolean]].value should be(true)
    ColumnTypeEquivalence.alignColumnTypes(intColumn, Some(10L).constant)._2.asInstanceOf[ConstantColumn[_]].value should be(10L)
    ColumnTypeEquivalence.alignColumnTypes(intColumn, Some(0.1).constant)._2.asInstanceOf[ConstantColumn[_]].value should be(0.1)
    ColumnTypeEquivalence.alignColumnTypes(intColumn, Some(BigDecimal("0.1")).constant)._2.asInstanceOf[ConstantColumn[_]].value should be(BigDecimal("0.1"))
    ColumnTypeEquivalence.alignColumnTypes(bigDecimalColumn, Some(10L).constant)._2.asInstanceOf[ConstantColumn[_]].value should be(10L)
    ColumnTypeEquivalence.alignColumnTypes(bigDecimalColumn, Some(0.1).constant)._2.asInstanceOf[ConstantColumn[_]].value should be(0.1)
    ColumnTypeEquivalence.alignColumnTypes(bigDecimalColumn, Some(BigDecimal("0.1")).constant)._2.asInstanceOf[ConstantColumn[_]].value should be(BigDecimal("0.1"))
    ColumnTypeEquivalence.alignColumnTypes(mappedYNBooleanColumn, Some(false).constant)._2.asInstanceOf[ConstantColumn[Boolean]].value should be(false)
    ColumnTypeEquivalence.alignColumnTypes(mapped10BooleanColumn, Some(true).constant)._2.asInstanceOf[ConstantColumn[Boolean]].value should be(true)
    ColumnTypeEquivalence.alignColumnTypes(wrappedStringColumn, Some(WrappedString("wrapping paper")).constant)._2.asInstanceOf[ConstantColumn[_]].value should be(WrappedString("wrapping paper"))
  }

  it should "throw an AssertionError for a None literal value which is equivalent to an non-Option" in {
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(stringColumn, Option.empty[String]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(booleanColumn, Option.empty[Boolean]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(intColumn, Option.empty[Int]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(intColumn, Option.empty[Int]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(intColumn, Option.empty[Int]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(bigDecimalColumn, Option.empty[BigDecimal]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(bigDecimalColumn, Option.empty[BigDecimal]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(bigDecimalColumn, Option.empty[BigDecimal]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(mappedYNBooleanColumn, Option.empty[Boolean]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(mapped10BooleanColumn, Option.empty[Boolean]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(wrappedStringColumn, Option.empty[WrappedString]) }

    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(stringColumn, Option.empty[String]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(booleanColumn, Option.empty[Boolean]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(intColumn, Option.empty[Int]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(intColumn, Option.empty[Int]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(intColumn, Option.empty[Int]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(bigDecimalColumn, Option.empty[BigDecimal]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(bigDecimalColumn, Option.empty[BigDecimal]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(bigDecimalColumn, Option.empty[BigDecimal]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(mappedYNBooleanColumn, Option.empty[Boolean]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(mapped10BooleanColumn, Option.empty[Boolean]) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(wrappedStringColumn, Option.empty[WrappedString]) }
  }

  it should "throw an AssertionError for two incompatible column types" in {
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(booleanColumn, mappedYNBooleanColumn) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(booleanColumn, mapped10BooleanColumn) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(mapped10BooleanColumn, mappedYNBooleanColumn) }

    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(booleanOptionColumn, mappedYNBooleanColumn) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(booleanOptionColumn, mapped10BooleanColumn) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(mapped10BooleanOptionColumn, mappedYNBooleanColumn) }

    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(booleanColumn, mappedYNBooleanOptionColumn) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(booleanColumn, mapped10BooleanOptionColumn) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(mapped10BooleanColumn, mappedYNBooleanOptionColumn) }

    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(booleanOptionColumn, mappedYNBooleanOptionColumn) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(booleanOptionColumn, mapped10BooleanOptionColumn) }
    intercept[AssertionError] { ColumnTypeEquivalence.alignColumnTypes(mapped10BooleanOptionColumn, mappedYNBooleanOptionColumn) }
  }

}