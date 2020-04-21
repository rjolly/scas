package scas.structure

trait Field[T] extends NotQuiteField[T] with NotQuiteGroup[T] {
  def (x: T).isUnit = !x.isZero
  def (x: T) / (y: T) = x * inverse(y)
}

object Field {
  def apply[T : Field] = summon[Field[T]]
}
