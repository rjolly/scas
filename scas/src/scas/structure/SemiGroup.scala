package scas.structure

trait SemiGroup[T] extends Structure[T] {
  extension (x: T) def * (y: T): T
}
