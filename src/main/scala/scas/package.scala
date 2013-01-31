import scas.structure.{Structure, AbelianGroup, SemiGroup, Monoid, Ring, UniqueFactorizationDomain}

package object scas {
  trait ExtraImplicits {
    implicit val ZZ = base.BigInteger
    implicit val QQ = base.Rational
    implicit val CC = base.Complex
  }
  object Implicits extends ExtraImplicits with scala.math.Ordering.ExtraImplicits with Structure.ExtraImplicits with AbelianGroup.ExtraImplicits with SemiGroup.ExtraImplicits with Monoid.ExtraImplicits with Ring.ExtraImplicits with UniqueFactorizationDomain.ExtraImplicits with PowerProduct.ExtraImplicits with Polynomial.ExtraImplicits with PolynomialWithGB.ExtraImplicits with MultivariatePolynomial.ExtraImplicits with UnivariatePolynomial.ExtraImplicits with RationalFunction.ExtraImplicits with Residue.ExtraImplicits with Module.ExtraImplicits

  type BigInteger = base.BigInteger
  type Rational = base.Rational
  type Complex = base.Complex
  type Variable = variable.Variable

  lazy val BigInteger = base.BigInteger
  lazy val Rational = base.Rational
  lazy val Complex = base.Complex

  val Variable = variable.Variable
  val ModInteger = base.ModInteger
  val Ordering = ordering.Ordering
  val PowerProduct = polynomial.PowerProduct
  val Polynomial = tree.Polynomial
  val PolynomialWithGB = tree.PolynomialWithGB
  val MultivariatePolynomial = tree.MultivariatePolynomial
  val UnivariatePolynomial = tree.UnivariatePolynomial
  val RationalFunction = quotient.RationalFunction
  val Residue = residue.Residue
  val Module = module.Module
  val Group = structure.Group
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
