package scas.structure.conversion

trait NotQuiteField[T] extends scas.structure.NotQuiteField[T] with Ring[T] {
  extension[U] (x: U)(using c: U => T) def / (y: T) = c(x).divide(y)
  extension (x: T) {
    def /[U](y: U)(using c: U => T) = x.divide(c(y))
  }
}
