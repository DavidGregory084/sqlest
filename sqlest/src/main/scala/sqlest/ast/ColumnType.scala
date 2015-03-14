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

import org.joda.time.DateTime
import sqlest.data.DataType
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/** Trait representing a column type read directly from the database. */
sealed trait NumericColumnType[A] extends BaseColumnType[A]
sealed trait NonNumericColumnType[A] extends BaseColumnType[A]

case object IntColumnType extends NumericColumnType[Int]
case object LongColumnType extends NumericColumnType[Long]
case object DoubleColumnType extends NumericColumnType[Double]
case object BigDecimalColumnType extends NumericColumnType[BigDecimal]

case object BooleanColumnType extends NonNumericColumnType[Boolean]
case object StringColumnType extends NonNumericColumnType[String]
case object DateTimeColumnType extends NonNumericColumnType[DateTime]
case object ByteArrayColumnType extends NonNumericColumnType[Array[Byte]]

case class MaterializeColumnTypeMacro(c: Context) {
  import c.universe._

  def materializeImpl[A: c.WeakTypeTag, B: c.WeakTypeTag] = {
    val typeOfA = c.weakTypeOf[A]
    val companion = typeOfA.typeSymbol.companion
    val applyMethod = findMethod(companion.typeSignature, "apply", typeOfA)
    val unapplyMethod = findMethod(companion.typeSignature, "unapply", typeOfA)
    val typeOfB = applyMethod.paramLists.head.head.asTerm.typeSignature
    val tableType = tq"sqlest.ast.Table"
    q"MappedDataType[$tableType, $typeOfA, $typeOfB](_.map($companion.$applyMethod), $companion.$unapplyMethod(_).get)"
  }

  def findMethod(companionType: Type, name: String, typeOfA: Type) = {
    val applyMethods = companionType.member(TermName(name)) match {
      case method: MethodSymbol => List(method)
      case termSymbol: TermSymbol => termSymbol.alternatives.collect { case method: MethodSymbol => method }
      case _ => Nil
    }

    val singleParamApplyMethods = applyMethods.filter(_.paramLists.flatten.length == 1)

    if (singleParamApplyMethods.length == 1) singleParamApplyMethods.head
    else c.abort(c.enclosingPosition, s"No matching $name method found on $typeOfA")
  }
}
