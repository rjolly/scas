package scas.structure.conversion

trait SemiGroup[T] extends scas.structure.SemiGroup[T] with Structure[T] {
  extension[U] (x: U)(using c: U => T) def * (y: T) = c(x).multiply(y)
  extension (x: T) {
    def *[U](y: U)(using c: U => T) = x.multiply(c(y))
  }
}
