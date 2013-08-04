package scas.structure.ordered

trait AbelianGroup[@specialized(Int, Long, Double) T] extends Structure[T] with scas.structure.AbelianGroup[T] {
  override def signum(x: T) = if (x < zero) -1 else if (x > zero) 1 else 0
}
