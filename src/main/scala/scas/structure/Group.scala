package scas.structure

trait Group[T] extends NotQuiteGroup[T] {
  override def isUnit(x: T) = true
}
