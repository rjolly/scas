package scas.application

import scas.structure.ordered.UniqueFactorizationDomain

trait OrderedUFDParsers[T] extends OrderedRingParsers[T] with UFDParsers[T] {
  implicit def structure: UniqueFactorizationDomain[T]
}
