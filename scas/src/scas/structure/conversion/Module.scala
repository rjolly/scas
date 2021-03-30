package scas.structure.conversion

trait Module[T, R: scas.structure.Ring] extends scas.structure.Module[T, R] with AbelianGroup[T] {
  extension[U] (x: U)(using c: U => R) def *%(y: T): T = c(x).multiplyLeft(y)
  extension (x: T) {
    def %*[U] (y: U)(using c: U => R): T = x.multiplyRight(c(y))
  }
}
