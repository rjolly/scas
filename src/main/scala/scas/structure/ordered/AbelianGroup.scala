package scas.structure.ordered

import scas.Implicits.infixOrderingOps

trait AbelianGroup[@specialized(Int, Long, Double) T] extends Structure[T] with scas.structure.AbelianGroup[T] {
  implicit def self: AbelianGroup[T]
  override def signum(x: T) = if (x < zero) -1 else if (x > zero) 1 else 0
}
