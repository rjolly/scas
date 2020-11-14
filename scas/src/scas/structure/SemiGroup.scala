package scas.structure

trait SemiGroup[T] extends Structure[T] {
  extension[U] (x: U)(using Conversion[U, T]) def * (y: T): T = (x: T) * y
  extension (x: T) def * (y: T): T
}
