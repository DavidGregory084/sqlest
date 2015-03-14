/**
 * Created by gregoryd on 13/03/2015.
 */

package sqlest.data

trait DataType[A, B] {
  type Database
  def read(database: Option[Database]): Option[B]
  def write(value: B): Database
}

trait BaseDataType[A, B] extends DataType[A, B] {
  type Database = B
  def read(database: Option[Database]) = database
  def write(value: B) = value
}

case class OptionDataType[A, B, C](nullValue: C, isNull: C => Boolean)(implicit innerDataType: DataType.Aux[A, B, C]) extends DataType[A, Option[B]] {
  type Database = C
  val baseDataType: BaseDataType[A, C] = innerDataType match {
    case baseDataType: DataType[A, B] with BaseDataType[A, C] => baseDataType
    case optionDataType: DataType[A, B] with OptionDataType[A, B, C] => optionDataType.baseDataType
    case mappedDataType: DataType[A, B] with MappedDataType[A, B, C] => mappedDataType.baseDataType
  }

  def read(database: Option[Database]): Option[Option[B]] = {
    if (database.map(isNull).getOrElse(false)) Some(None)
    else Some(innerDataType.read(database))
  }

  def write(value: Option[B]): Database =
    if (value.isEmpty) nullValue
    else innerDataType.write(value.get)
}

object OptionDataType {
  def apply[A, B, C](nullValue: C)(implicit innerDataType: DataType.Aux[A, B, C]): OptionDataType[A, B, C] = apply(nullValue, (_: C) == nullValue)
  def apply[A, B, C](innerDataType: DataType.Aux[A, B, C]): OptionDataType[A, B, C] = apply(null.asInstanceOf[C])(innerDataType)
}

trait MappedDataType[A, B, C] extends DataType[A, B] {
  type Database = C
  val baseDataType: BaseDataType[A, C]
}

object MappedDataType {
  def apply[A, B, C](r: Option[C] => Option[B], w: B => C)(implicit innerDataType: DataType.Aux[A, C, C]) = new MappedDataType[A, B, C] {
    val baseDataType = innerDataType.asInstanceOf[BaseDataType[A, C]]
    def read(database: Option[Database]) = r(innerDataType.read(database))
    def write(value: B) = innerDataType.write(w(value))
  }

  implicit class MappedDataTypeOps[A, B, C](mappedDataType: MappedDataType[A, B, C]) {
    def compose[D](inner: DataType.Aux[A, C, D])(implicit baseDataType: BaseDataType[A, D]): MappedDataType[A, B, D] = {
      MappedDataType((database: Option[D]) => mappedDataType.read(inner.read(database)), (value: B) => inner.write(mappedDataType.write(value)))
    }
  }
}

object DataType {
  type Aux[A, B, C] = DataType[A, B] { type Database = C }

  implicit class OptionDataTypeOps[A, B, C](left: DataType.Aux[A, B, C]) {
    def toOptionDataType = left match {
      case option: DataType[A, B] with OptionDataType[A, B, C] => option
      case base => OptionDataType.apply[A, B, base.Database](base)
    }
  }
}

