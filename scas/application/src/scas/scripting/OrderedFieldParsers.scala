package scas.scripting

import scala.compiletime.deferred

trait OrderedFieldParsers[T] extends UFDParsers[T] with OrderedUFDParsers[T] {
  given structure: scas.structure.commutative.ordered.Field[T] = deferred
}
