package zio.intellij.utils.types

trait Type {
  def name: String
  def fqName: String
}

trait TypeCompanion[T <: Type] {
  def values: List[T]
  def defaultValue: T

  def fromNameOpt(name: String): Option[T] = values.find(_.name == name)
  def fromName(name: String): T            = fromNameOpt(name).getOrElse(defaultValue)

  def fromFQNameOpt(fqName: String): Option[T] = values.find(_.fqName == fqName)
  def fromFQName(fqName: String): T            = fromFQNameOpt(fqName).getOrElse(defaultValue)
}
