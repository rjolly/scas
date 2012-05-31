package scas.application

import scas._

object MyApp extends App {
  pp1
  pp2
  polynomial
  solvablePolynomial
  bigint
  modint
  modPolynomial
  product
  rational
  rationalPolynomial
  univariatePolynomial
  rf
  complex
  an
  module
  gcdSubres
  gcdMultivariate

  def pp1 = {
    import Implicits.{infixOrderingOps, infixPowerProductOps}
    implicit val m = PowerProduct('x)
    val Array(x) = m.generators
    assert (x > m(1))
    assert (m(1) < x)
    assert (m(1) | x)
    assert (x * m(1) >< x)
    assert (x * x >< pow(x, 2))
  }

  def pp2 = {
    import Implicits.infixPowerProductOps
    implicit val m = PowerProduct((for (i <- 0 until 4) yield (for (j <- 0 until 2) yield Variable("a", i, j)).toArray).toArray, Ordering.lexicographic[Int])
    val a = m.generatorsBy(2)
    val s = (for (i <- 0 until 4) yield (for (j <- 0 until 2) yield a(i)(j).toCode(0)).toArray).toArray
    assert (s.deep.toString == "Array(Array(a(0)(0), a(0)(1)), Array(a(1)(0), a(1)(1)), Array(a(2)(0), a(2)(1)), Array(a(3)(0), a(3)(1)))");
  }

  def polynomial = {
    import Implicits.{ZZ, coef2polynomial}
    implicit val r = Polynomial(ZZ, 'x)
    implicit val s = Polynomial(r, 'y)
    val Array(x) = r.generators
    val Array(y) = s.generators
    assert (x + 1 >< 1 + x)
    assert (x + BigInteger(1) >< BigInteger(1) + x)
    assert (y + x >< x + y)
    assert (y + 1 >< 1 + y)
    assert (y + BigInteger(1) >< BigInteger(1) + y)
  }

  def solvablePolynomial = {
    import Implicits.ZZ
    implicit val r = Polynomial.weylAlgebra(ZZ, PowerProduct('a, 'x, 'b, 'y))
    val Array(a, x, b, y) = r.generators
    assert (b*a+y*x >< 2+a*b+x*y)
    assert (r.toString == "ZZ[a, x, b, y][[b*a = 1+a*b], [y*x = 1+x*y]]")
  }

  def bigint = {
    import Implicits.{ZZ, infixOrderingOps, infixRingOps}
    val a = BigInteger(1)
    val b = a + a
    val c = pow(b, 32)
    val d = pow(c, 2)
    val e = BigInteger("18446744073709551616")
    assert (b >< BigInteger(2))
    assert (BigInteger(2) >< b)
    assert (a + BigInteger(1) >< b)
    assert (BigInteger(1) + a >< b)
    assert (b > BigInteger(1))
    assert (BigInteger(1) < b)
    assert (b.toCode(0) == "2")
    assert (c.toCode(0) == "4294967296l")
    assert (d.toCode(0) == "BigInteger(\"18446744073709551616\")")
    assert (d >< e)
  }

  def modint = {
    import Implicits.infixRingOps
    implicit val r = ModInteger(7)
    val a = r(4)
    val b = a + a
    val c = pow(a, 2)
    assert (b >< r(1))
    assert (r(1) >< b)
    assert (b.toString == "1")
    assert (c.toString == "2")
    assert (r.toString == "ZZ(7)")
    assert (r.characteristic.intValue == 7)
  }

  def modPolynomial = {
    implicit val r = ModInteger(2)
    implicit val s = Polynomial(r, 'x)
    val Array(x) = s.generators
    assert (1 + x + 1 >< x)
    assert (s.toString == "ZZ(2)[x]")
  }

  def product = {
    implicit val r = Product("r", ModInteger(3), ModInteger(5))
    val a = r(1, 3)
    val b = a + a
    assert (b >< r(2, 1))
    assert (r.toString == "ZZ(3)*ZZ(5)")
    assert (r.characteristic.intValue == 15)
  }

  def rational = {
    import Implicits.{QQ, infixUFDOps}
    assert (Rational(1) + frac(1, 2) >< frac(1, 2) + Rational(1))
    assert (frac(1, 2) + frac(3, 4) >< frac(5, 4))
    assert (pow(frac(3, 2), 2) >< frac(9, 4))
  }

  def rationalPolynomial = {
    import Implicits.{QQ, coef2polynomial}
    implicit val r = Polynomial(QQ, 'x)
    val Array(x) = r.generators
    assert (x + frac(1, 2) >< frac(1, 2) + x)
    assert (x + 1 >< 1 + x)
  }

  def univariatePolynomial = {
    import Implicits.{QQ, coef2univariatePolynomial}
    implicit val r = UnivariatePolynomial(QQ, "x")
    import r.{generators, gcd, modInverse}
    val Array(x) = generators
    assert (gcd((1+x)*(1+frac(1, 2)*x), (1+frac(1, 2)*x)*(1-x)) >< 2+x)
    assert (modInverse(1-x, pow(1+x, 2)) >< frac(3, 4)+frac(1, 4)*x)
  }

  def rf = {
    import Implicits.{QQ, coef2rationalFunction}
    implicit val q = RationalFunction(QQ, "x")
    val Array(x) = q.generators
    assert (x + frac(1, 2) >< frac(1, 2) + x)
    assert (x + 1 >< 1 + x)
    assert ((pow(x, 2) - 1)/(x - 1) >< x + 1)
    assert ((1 - pow(x, 2))/(1 - x) >< 1 + x)
    assert ((x/(2 * x)).toString == "frac(1, 2)")
    assert (q.toString == "QQ(x)")
  }

  def complex = {
    import Implicits.CC
    assert ((1+I)/(1-I) >< I)
  }

  def an = {
    import Implicits.{QQ, coef2residue}
    implicit val r = Residue(QQ, "x")
    val Array(x) = r.generators
    r.update(2 - pow(x, 2))
    assert (2 >< pow(x, 2))
    assert (Rational(2) - pow(x, 2) >< 0)
    assert (r.toString == "QQ(2-pow(x, 2))")
  }

  def module = {
    import Implicits.{QQ, coef2polynomial, ring2scalar}
    implicit val r = Polynomial(QQ, 'x)
    implicit val m = Module("e", 2, r)
    val Array(x) = r.generators
    val e = m.generators
    assert (2 * e(0) >< e(0) * 2)
    assert (frac(1, 2) * e(0) >< e(0) * frac(1, 2))
    assert (x * e(0) >< e(0) * x)
    assert (2 * x * e(0) >< e(0) * 2 * x)
    assert (frac(1, 2) * x * e(0) >< e(0) * frac(1, 2) * x)
    assert (e(0) + e(1) >< e(0) + e(1))
    assert ((2 * e(0) + e(1)).toCode(0) == "2*e(0)+e(1)")
    assert ((frac(1, 2) * e(0) + e(1)).value.deep.toString == "Array(frac(1, 2), 1)")
    assert (m.toString == "QQ[x]^2")
  }

  def gcdSubres = {
    import Implicits.ZZ
    implicit val r = MultivariatePolynomial(ZZ, PowerProduct('x))
    import r.{generators, gcd}
    val Array(x) = generators
    assert (gcd(0, 0) >< 0)
    assert (gcd(x, 0) >< x)
    assert (gcd(1, x) >< 1)
    assert (gcd((1+x)*(1+x), (1+x)*(1-x)) >< 1+x)
  }

  def gcdMultivariate = {
    import Implicits.ZZ
    implicit val r = MultivariatePolynomial(ZZ, PowerProduct('x, 'y, 'z))
    import r.{generators, gcd}
    val Array(x, y, z) = generators
    assert (gcd(x*y, x*z) >< x)
  }
}
