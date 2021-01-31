package scas.structure

trait NotQuiteField[T] extends Ring[T] {
  extension[U] (x: U)(using c: U => T) def / (y: T): T = c(x).divide(y)
  extension (x: T) {
    def /[U](y: U)(using c: U => T): T = x.divide(c(y))
    def divide(y: T): T
  }
}
