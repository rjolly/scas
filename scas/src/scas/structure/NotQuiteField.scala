package scas.structure

trait NotQuiteField[T] extends Ring[T] {
  extension[U] (x: U)(using c: U => T) def / (y: T): T = c(x) / y
  extension (x: T) def / (y: T): T
}
