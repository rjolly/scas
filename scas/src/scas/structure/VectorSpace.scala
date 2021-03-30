package scas.structure

trait VectorSpace[T, R: Field] extends Module[T, R] {
  extension (x: T) {
    def %/ (y: R) = x%* Field[R].inverse(y)
  }
}
