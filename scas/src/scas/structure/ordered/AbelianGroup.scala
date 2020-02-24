package scas.structure.ordered

trait AbelianGroup[T] extends scas.structure.AbelianGroup[T] with Structure[T] with
  def signum(x: T) = if (x < zero) -1 else if (x > zero) 1 else 0
