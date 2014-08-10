package scas.structure.ordered

trait Ring[@specialized(Int, Long, Double) T] extends AbelianGroup[T] with Monoid[T] with scas.structure.Ring[T] {
  implicit def self: Ring[T]
}
