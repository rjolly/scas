package scas.structure

trait SemiGroup[T] extends Structure[T] {
  extension[U] (x: U)(using c: U => T) def * (y: T): T = c(x).multiply(y)
  extension (x: T) {
    def *[U](y: U)(using c: U => T): T = x.multiply(c(y))
    def multiply(y: T): T
  }
}
