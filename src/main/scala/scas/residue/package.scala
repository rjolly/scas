package scas

package object residue {
  trait ExtraImplicits {
    implicit val CC = Complex
  }
  object Implicits extends ExtraImplicits

  type Complex = Residue.Element[UnivariatePolynomial.Element[Rational, Int], Rational, Int]

  object Complex extends ComplexLike

  def sqrt(x: Complex) = Complex.sqrt(x)
}
