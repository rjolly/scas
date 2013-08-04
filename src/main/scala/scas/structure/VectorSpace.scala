package scas.structure

trait VectorSpace[T <: (Int => R), R] extends Module[T, R] {
  implicit val ring: Field[R]
}
