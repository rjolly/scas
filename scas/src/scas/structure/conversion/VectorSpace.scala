package scas.structure.conversion

abstract class VectorSpace[T, R: scas.structure.Field] extends Module[T, R] with scas.structure.VectorSpace[T, R] {
  extension (x: T) {
    def %/[U] (y: U)(using c: U => R) = super.%/(x)(c(y))
  }
}
