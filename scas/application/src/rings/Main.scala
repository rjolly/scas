package rings

import scala.util.FromDigits
import cc.redberry.rings.Ring
import cc.redberry.rings.Rings
import cc.redberry.rings.Integers
import cc.redberry.rings.poly.MultivariateRing
import cc.redberry.rings.poly.multivar.MultivariatePolynomial
import cc.redberry.rings.poly.multivar.DegreeVector;
import java.util.Comparator

type BigInteger = cc.redberry.rings.bigint.BigInteger

given Z as Integers = Rings.Z

given BigInteger as RdbRing[BigInteger] with FromDigits[BigInteger] {
  def fromDigits(digits: String) = new BigInteger(digits)
}

type MultivariatePolynomialRing[C] = MultivariateRing[MultivariatePolynomial[C]]

object MultivariatePolynomialRing {
  def apply[C : MultivariatePolynomialRing] = summon[MultivariatePolynomialRing[C]]
  def apply[E](nVariables: Int, coefficientRing: Ring[E], monomialOrder: Comparator[DegreeVector]) = MultivariateRing(MultivariatePolynomial.zero(nVariables, coefficientRing, monomialOrder))
}

given poly2scas[C : MultivariatePolynomialRing] as RdbRing[MultivariatePolynomial[C]] {
  def (factory: MultivariatePolynomialRing[C]) gens = (for (i <- 0 until factory.nVariables()) yield factory.variable(i)).toArray
}

given id[T] as Conversion[T, T] = identity
given int2bigInt as Conversion[Int, BigInteger] = cc.redberry.rings.bigint.BigInteger.valueOf(_)
given long2bigInt as Conversion[Long, BigInteger] = cc.redberry.rings.bigint.BigInteger.valueOf(_)
given coef2poly[U, C : MultivariatePolynomialRing](using Conversion[U, C]) as Conversion[U, MultivariatePolynomial[C]] = MultivariatePolynomialRing[C].factory().createConstant(_)

given bigInt2scas[U](using Conversion[U, BigInteger]) as Conversion[U, scas.BigInteger] = (x: U) => java.math.BigInteger(x.toByteArray)

def (a: Long) \:(b: Long) = BigInteger(a) \ b
