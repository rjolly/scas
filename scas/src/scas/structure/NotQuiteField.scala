package scas.structure

trait NotQuiteField[T] extends Ring[T] {
  extension (x: T) def / (y: T): T
}
