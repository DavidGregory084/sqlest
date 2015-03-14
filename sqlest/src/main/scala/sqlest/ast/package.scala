/**
 * Created by gregoryd on 13/03/2015.
 */

package sqlest

import sqlest.data._
import scala.language.experimental.macros

package object ast {
  type ColumnType[A] = DataType[Table, A]
  type BaseColumnType[A] = BaseDataType[Table, A]
  type OptionColumnType[A, B] = OptionDataType[Table, A, B]
  type MappedColumnType[A, B] = MappedDataType[Table, A, B]

  object ColumnType { type Aux[A, B] = ColumnType[A] { type Database = B } }

  implicit def apply[A, B]: MappedColumnType[A, B] = macro MaterializeColumnTypeMacro.materializeImpl[A, B]
  implicit def optionType[A, B](implicit base: ColumnType.Aux[A, B]): OptionColumnType[A, B] = OptionDataType[Table, A, B](base)

  implicit val booleanColumnType = BooleanColumnType
  implicit val intColumnType = IntColumnType
  implicit val longColumnType = LongColumnType
  implicit val doubleColumnType = DoubleColumnType
  implicit val bigDecimalColumnType = BigDecimalColumnType
  implicit val stringColumnType = StringColumnType
  implicit val dateTimeColumnType = DateTimeColumnType
  implicit val byteArrayColumnType = ByteArrayColumnType
}
