package scas.application

import scas.structure.ordered.Ring

trait OrderedRingParsers[T] extends RingParsers[T] with OrderingParsers[T] {
  implicit def structure: Ring[T]
}
