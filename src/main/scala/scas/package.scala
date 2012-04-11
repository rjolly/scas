import scas.structure.{Monoid, Ring, UniqueFactorizationDomain}
import scas.polynomial.PowerProduct

package object scas {
  trait ExtraImplicits {
    implicit val ZZ = scas.base.BigInteger
    implicit val QQ = scas.base.Rational
    implicit val CC = scas.base.Complex
  }
  object Implicits extends ExtraImplicits with Ordering.ExtraImplicits with Ring.ExtraImplicits with UniqueFactorizationDomain.ExtraImplicits with PowerProduct.ExtraImplicits

  val BigInteger = scas.base.BigInteger
  val ModInteger = scas.base.ModInteger
  lazy val frac = scas.base.Rational
  lazy val I = scas.base.Complex.I
  val Lexicographic = scas.polynomial.ordering.Lexicographic
  val DegreeLexicographic = scas.polynomial.ordering.DegreeLexicographic
  val DegreeReverseLexicographic = scas.polynomial.ordering.DegreeReverseLexicographic
  val KthElimination = scas.polynomial.ordering.KthElimination
  val PowerProduct = scas.polynomial.PowerProduct
  val Polynomial = scas.polynomial.tree.Polynomial
  val SolvablePolynomial = scas.polynomial.tree.SolvablePolynomial
  val WeylAlgebra = scas.polynomial.tree.WeylAlgebra
  val MultivariatePolynomial = scas.polynomial.ufd.tree.MultivariatePolynomial
  val PolynomialWithSimpleGCD = scas.polynomial.ufd.tree.PolynomialWithSimpleGCD
  val PolynomialWithPrimitiveGCD = scas.polynomial.ufd.tree.PolynomialWithPrimitiveGCD
  val PolynomialWithSubresGCD = scas.polynomial.ufd.tree.PolynomialWithSubresGCD
  val UnivariatePolynomial = scas.polynomial.ufd.tree.int.UnivariatePolynomial
  val AlgebraicNumber = scas.polynomial.ufd.tree.int.AlgebraicNumber
  val RationalFunction = scas.polynomial.ufd.RationalFunction
  val Product = scas.structure.Product
  val Module = scas.module.Module
  implicit def int2bigInteger(i: Int) = java.math.BigInteger.valueOf(i)
  implicit def int2bigIntegerOps(i: Int)(implicit factory: UniqueFactorizationDomain[java.math.BigInteger]): UniqueFactorizationDomain[java.math.BigInteger]#Ops = factory.mkOps(i)
  implicit def int2bigIntegerOrderingOps(i: Int)(implicit factory: UniqueFactorizationDomain[java.math.BigInteger]): Ordering[java.math.BigInteger]#Ops = factory.mkOrderingOps(i)
  implicit def long2bigInteger(l: Long) = java.math.BigInteger.valueOf(l)
  implicit def long2bigIntegerOps(l: Long)(implicit factory: UniqueFactorizationDomain[java.math.BigInteger]): UniqueFactorizationDomain[java.math.BigInteger]#Ops = factory.mkOps(l)
  implicit def long2bigIntegerOrderingOps(l: Long)(implicit factory: UniqueFactorizationDomain[java.math.BigInteger]): Ordering[java.math.BigInteger]#Ops = factory.mkOrderingOps(l)
  implicit def int2powerProduct[N: PowerProduct](i: Int) = implicitly[PowerProduct[N]].apply(i)
  implicit def int2powerProductOps[N: PowerProduct](i: Int) = implicitly[PowerProduct[N]].mkOps(i)
  implicit def int2powerProductOrderingOps[N: PowerProduct](i: Int) = implicitly[PowerProduct[N]].mkOrderingOps(i)
  def pow[T: Monoid](x: T, exp: java.math.BigInteger) = implicitly[Monoid[T]].pow(x, exp)
  implicit val ordering = Lexicographic[Int]
  implicit val random = new java.util.Random()
}
