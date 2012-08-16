import scas.structure.{Structure, AbelianGroup, SemiGroup, Monoid, Ring, UniqueFactorizationDomain}

package object scas {
  trait ExtraImplicits {
    implicit val ZZ = BigInteger
    implicit val QQ = Rational
    implicit val CC = Complex
  }
  object Implicits extends ExtraImplicits with scala.math.Ordering.ExtraImplicits with Structure.ExtraImplicits with AbelianGroup.ExtraImplicits with SemiGroup.ExtraImplicits with Monoid.ExtraImplicits with Ring.ExtraImplicits with UniqueFactorizationDomain.ExtraImplicits with PowerProduct.ExtraImplicits with Polynomial.ExtraImplicits with PolynomialWithGB.ExtraImplicits with MultivariatePolynomial.ExtraImplicits with UnivariatePolynomial.ExtraImplicits with RationalFunction.ExtraImplicits with Residue.ExtraImplicits with Module.ExtraImplicits

  type BigInteger = java.math.BigInteger
  type Rational = (BigInteger, BigInteger)
  type Complex = Residue.Element[UnivariatePolynomial.Element[Rational, Int], Rational, Int]
  type Variable = variable.Variable

  object BigInteger extends base.BigIntegerLike
  object Rational extends base.RationalLike
  object Complex extends base.ComplexLike

  val Variable = variable.Variable
  val ModInteger = base.ModInteger
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
  implicit def string2bigInteger(s: String) = BigInteger(s)
  implicit def bigInteger2rational[A <% BigInteger](value: A) = Rational(value)

  def pow[T: Monoid](x: T, exp: BigInteger) = implicitly[Monoid[T]].pow(x, exp)
  def frac(n: BigInteger, d: BigInteger) = Rational(n, d)
  def sqrt(x: Complex) = Complex.sqrt(x)
}
