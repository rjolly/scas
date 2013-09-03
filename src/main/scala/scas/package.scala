import scas.math.Ordering
import scas.structure.{Monoid, StarRingWithUFD, BooleanAlgebraWithUFD}

package object scas {
  trait ExtraImplicits {
    implicit val ZZ = BigInteger
    implicit val QQ = Rational
    implicit val RR = Double
    implicit val CC = Complex
    implicit val ZZ2 = Boolean
    implicit val R2R = Function
  }
  object Implicits extends ExtraImplicits with Ordering.ExtraImplicits with StarRingWithUFD.ExtraImplicits with BooleanAlgebraWithUFD.ExtraImplicits with PowerProduct.ExtraImplicits with Polynomial.ExtraImplicits with PolynomialWithGB.ExtraImplicits with MultivariatePolynomial.ExtraImplicits with UnivariatePolynomial.ExtraImplicits with RationalFunction.ExtraImplicits with Residue.ExtraImplicits with Module.ExtraImplicits with Vector.ExtraImplicits with Matrix.ExtraImplicits

  type BigInteger = base.BigInteger
  type Rational = base.Rational
  type Complex = base.Complex
  type Variable = variable.Variable

  lazy val BigInteger = base.BigInteger
  lazy val Rational = base.Rational
  lazy val Double = base.Double
  lazy val Complex = base.Complex
  lazy val Boolean = base.Boolean
  lazy val Function = base.Function

  val Variable = variable.Variable
  val ModInteger = base.ModInteger
  val PowerProduct = power.PowerProduct
  val Polynomial = polynomial.tree.Polynomial
  val PolynomialWithGB = polynomial.tree.PolynomialWithGB
  val MultivariatePolynomial = polynomial.tree.MultivariatePolynomial
  val UnivariatePolynomial = polynomial.tree.UnivariatePolynomial
  val RationalFunction = quotient.RationalFunction
  val Residue = residue.Residue
  val Module = module.Module
  val Vector = module.Vector
  val Matrix = module.Matrix
  val Group = structure.Group
  val Product = structure.Product

  implicit val random = new java.util.Random()
  implicit def int2bigInteger(i: Int) = java.math.BigInteger.valueOf(i)
  implicit def long2bigInteger(l: Long) = java.math.BigInteger.valueOf(l)
  implicit def string2bigInteger(s: String) = BigInteger(s)
  implicit def bigInteger2rational[A <% BigInteger](value: A) = Rational(value)
  implicit def double2complex[A <% Double](value: A) = Complex(value)

  def pow[T: Monoid](x: T, exp: BigInteger) = implicitly[Monoid[T]].pow(x, exp)
  def frac(n: BigInteger, d: BigInteger) = Rational(n, d)
  def sqrt(x: Complex) = Complex.sqrt(x)
}
