package scas.structure.ordered

trait UniqueFactorizationDomain[@specialized(Int, Long, Double) T] extends Ring[T] with scas.structure.UniqueFactorizationDomain[T] {
  implicit def self: UniqueFactorizationDomain[T]
}
