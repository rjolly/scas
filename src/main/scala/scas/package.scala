import scas.structure.{Monoid, Ring, UniqueFactorizationDomain}

package object scas {
  trait ExtraImplicits {
    implicit val ZZ = base.BigInteger
    implicit val QQ = base.Rational
    implicit val CC = base.Complex
  }
  object Implicits extends ExtraImplicits with Ordering.ExtraImplicits with Ring.ExtraImplicits with UniqueFactorizationDomain.ExtraImplicits with PowerProduct.ExtraImplicits with Polynomial.ExtraImplicits with SolvablePolynomial.ExtraImplicits with UnivariatePolynomial.ExtraImplicits with MultivariatePolynomial.ExtraImplicits with AlgebraicNumber.ExtraImplicits with RationalFunction.ExtraImplicits with Module.ExtraImplicits

  val BigInteger = base.BigInteger
  val ModInteger = base.ModInteger
  lazy val Rational = base.Rational
  lazy val frac = base.Rational
  lazy val I = base.Complex.I
  val Lexicographic = polynomial.ordering.Lexicographic
  val DegreeLexicographic = polynomial.ordering.DegreeLexicographic
  val DegreeReverseLexicographic = polynomial.ordering.DegreeReverseLexicographic
  val KthElimination = polynomial.ordering.KthElimination
  val PowerProduct = polynomial.PowerProduct
  val Polynomial = polynomial.tree.Polynomial
  val SolvablePolynomial = polynomial.tree.SolvablePolynomial
  val MultivariatePolynomial = polynomial.tree.MultivariatePolynomial
  val UnivariatePolynomial = polynomial.tree.UnivariatePolynomial
  val AlgebraicNumber = polynomial.tree.AlgebraicNumber
  val RationalFunction = polynomial.RationalFunction
  val Module = module.Module
  val Product = structure.Product
  implicit val random = new java.util.Random()
  implicit def int2bigInteger(i: Int) = java.math.BigInteger.valueOf(i)
  implicit def long2bigInteger(l: Long) = java.math.BigInteger.valueOf(l)
  implicit def bigInteger2rational[A <% java.math.BigInteger](value: A) = Rational(value)
  def pow[T: Monoid](x: T, exp: java.math.BigInteger) = implicitly[Monoid[T]].pow(x, exp)
}
