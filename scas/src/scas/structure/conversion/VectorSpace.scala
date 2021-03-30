package scas.structure.conversion

trait VectorSpace[T, R: scas.structure.Field] extends scas.structure.VectorSpace[T, R] with Module[T, R] {
  extension (x: T) {
    def %/[U] (y: U)(using c: U => R) = super.%/(x)(c(y))
  }
}
