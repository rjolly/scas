package scas.scripting

import scala.compiletime.deferred

trait OrderedUFDParsers[T] extends UFDParsers[T] with OrderedRingParsers[T] {
  given structure: () => scas.structure.commutative.ordered.UniqueFactorizationDomain[T] = deferred
}
