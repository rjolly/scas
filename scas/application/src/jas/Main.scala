package jas

import scala.util.FromDigits
import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scala.collection.JavaConverters.asScalaBufferConverter

type BigInteger = edu.jas.arith.BigInteger

given ZZ as BigInteger = new BigInteger()

given BigInteger as JasRing[BigInteger] with FromDigits[BigInteger] {
  def fromDigits(digits: String) = new BigInteger(digits)
  def apply(x: BigInteger) = x
}

given poly2scas[C <: RingElem[C] : GenPolynomialRing] as JasRing[GenPolynomial[C]] {
  def (factory: GenPolynomialRing[C]) gens = factory.getGenerators().asScala.toArray
}

given id[T] as Conversion[T, T] = identity
given int2bigInt as Conversion[Int, BigInteger] = new BigInteger(_)
given long2bigInt as Conversion[Long, BigInteger] = new BigInteger(_)
given coef2poly[U, C <: RingElem[C] : GenPolynomialRing](using Conversion[U, C]) as Conversion[U, GenPolynomial[C]] = summon[GenPolynomialRing[C]].valueOf(_)

given bigInt2scas[U](using Conversion[U, BigInteger]) as Conversion[U, scas.BigInteger] = _.`val`

def (a: Long) \:(b: Long) = BigInteger(a) \ b
