package scas.structure

trait NotQuiteField[T] extends Ring[T] {
  extension[U] (x: U)(using Conversion[U, T]) def / (y: T): T = (x: T) / y
  extension (x: T) def / (y: T): T
}
