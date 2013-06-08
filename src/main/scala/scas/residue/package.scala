package scas

package object residue {
  type Complex = Residue.Element[UnivariatePolynomial.Element[Rational, Int], Rational, Int]

  object Complex extends ComplexLike
}
