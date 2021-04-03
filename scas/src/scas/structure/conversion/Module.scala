package scas.structure.conversion

abstract class Module[T, R: scas.structure.Ring] extends AbelianGroup[T] with scas.structure.Module[T, R] {
  extension[U] (x: U)(using c: U => R) def *%(y: T) = c(x).multiplyLeft(y)
  extension (x: T) {
    def %*[U] (y: U)(using c: U => R) = x.multiplyRight(c(y))
  }
}
