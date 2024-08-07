package scas.scripting 

trait OrderedRingParsers[T] extends RingParsers[T] with OrderingParsers[T] {
  given structure: scas.structure.ordered.Ring[T]
}
