package scas.scripting

trait OrderedUFDParsers[T] extends UFDParsers[T] with OrderedRingParsers[T] {
  given structure: scas.structure.commutative.ordered.UniqueFactorizationDomain[T]
}
