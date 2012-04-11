package scas.polynomial.ufd

import scas.structure.Quotient
import scas.Implicits.infixUFDOps
import Quotient.Element

class RationalFunction[R <: PolynomialOverUFD.Element[R, C, N], C, N](override val ring: PolynomialOverField[R, C, N]) extends Quotient[R]()(ring) {
  implicit val rr = ring.ring
  def generator(n: Int) = apply(ring.generator(n))
  def generators = (for (i <- 0 until length) yield generator(i)).toArray
  def generatorsBy(n: Int) = {
    val m = length/n
    (for (i <- 0 until m) yield (for (j <- 0 until n) yield generator(i * n + j)).toArray).toArray
  }
  override def random(numbits: Int)(implicit rnd: java.util.Random) = zero
  override def toCode(x: Element[R], precedence: Int) = {
    val Element(n, d) = x
    if (ring.isOne(d)) ring.toCode(n, precedence)
    else {
      if (ring.degree(n) == 0 && ring.degree(d) == 0) {
        val nn = ring.headCoefficient(n)
        val dd = ring.headCoefficient(d)
        rr.toCode(nn / dd, precedence)
      } else {
        val s = ring.toCode(n, 2) + "/" + ring.toCode(d, 2)
        val fenced = precedence > 1
        if (fenced) "(" + s + ")" else s
      }
    }
  }
  override def toString = rr.toString + "(" + variables.mkString(", ") + ")"

  def variables = ring.variables

  def length = variables.length
}

object RationalFunction {
  def apply[R <: PolynomialOverUFD.Element[R, C, N], C, N](ring: PolynomialOverField[R, C, N]) = new RationalFunction(ring)
}
