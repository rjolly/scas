package scas.structure

trait Field[T] extends Ring[T] with NotQuiteGroup[T] with
  def (x: T).isUnit = x <> zero
  def (x: T) / (y: T) = x * inverse(y)
