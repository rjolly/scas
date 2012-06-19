import scas.structure.{Structure, AbelianGroup, SemiGroup, Monoid, Ring, UniqueFactorizationDomain}

package object scas {
  trait ExtraImplicits {
    implicit val ZZ = base.BigInteger
    implicit val QQ = base.Rational
    implicit val CC = base.Complex
  }
  object Implicits extends ExtraImplicits with scala.math.Ordering.ExtraImplicits with Structure.ExtraImplicits with AbelianGroup.ExtraImplicits with SemiGroup.ExtraImplicits with Monoid.ExtraImplicits with Ring.ExtraImplicits with UniqueFactorizationDomain.ExtraImplicits with PowerProduct.ExtraImplicits with Polynomial.ExtraImplicits with PolynomialWithGB.ExtraImplicits with MultivariatePolynomial.ExtraImplicits with UnivariatePolynomial.ExtraImplicits with RationalFunction.ExtraImplicits with Residue.ExtraImplicits with Module.ExtraImplicits

  val BigInteger = base.BigInteger
  val ModInteger = base.ModInteger
  lazy val Rational = base.Rational
  lazy val frac = base.Rational
  val Ordering = polynomial.ordering.Ordering
  val PowerProduct = polynomial.PowerProduct
  val Polynomial = polynomial.tree.Polynomial
  val PolynomialWithGB = polynomial.tree.PolynomialWithGB
  val MultivariatePolynomial = polynomial.tree.MultivariatePolynomial
  val UnivariatePolynomial = polynomial.tree.UnivariatePolynomial
  val RationalFunction = polynomial.quotient.RationalFunction
  val Residue = polynomial.residue.Residue
  val Module = module.Module
  val Product = structure.Product
  implicit val random = new java.util.Random()
  implicit def int2bigInteger(i: Int) = java.math.BigInteger.valueOf(i)
  implicit def long2bigInteger(l: Long) = java.math.BigInteger.valueOf(l)
  implicit def bigInteger2rational[A <% java.math.BigInteger](value: A) = Rational(value)
  def pow[T: Monoid](x: T, exp: java.math.BigInteger) = implicitly[Monoid[T]].pow(x, exp)
  def sqrt(x: Residue.Element[
    UnivariatePolynomial.Element[
      (java.math.BigInteger, java.math.BigInteger),
      Int
   ],
    (java.math.BigInteger, java.math.BigInteger),
    Int
  ]) = base.Complex.sqrt(x)
}
