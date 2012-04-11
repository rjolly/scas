package scas.structure

import scas.Implicits.infixUFDOps
import Quotient.Element

class Quotient[R](implicit val ring: UniqueFactorizationDomain[R]) extends Field[Element[R]] {
  def apply(x: Element[R]) = {
    val Element(n, d) = x
    reduce(n, d)
  }
  def reduce(n: R, d: R) = {
    val gcd = ring.gcd(n, d) match { case gcd => if (ring.signum(d) < 0) -gcd else gcd }
    apply(n / gcd, d / gcd)
  }
  def apply(n: R, d: R) = new Element(n, d)(this)
  def apply(n: R): Element[R] = apply(n, ring.one)
  def apply(l: Long) = apply(ring(l))
  def random(numbits: Int)(implicit rnd: java.util.Random) = {
    val n = ring.random(numbits)
    val d = ring.random(numbits)
    reduce(if (rnd.nextBoolean()) -n else n, d + ring.one)
  }
  override def pow(x: Element[R], exp: java.math.BigInteger) = if (exp.signum() < 0) pow(inverse(x), exp.negate()) else {
    val Element(n, d) = x
    apply(ring.pow(n, exp), ring.pow(d, exp))
  }
  override def abs(x: Element[R]) = {
    val Element(n, d) = x
    apply(ring.abs(n), d)
  }
  override def signum(x: Element[R]) = {
    val Element(n, d) = x
    ring.signum(n)
  }
  def characteristic = ring.characteristic
  def plus(x: Element[R], y: Element[R]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    reduce(a * d + c * b, b * d)
  }
  def minus(x: Element[R], y: Element[R]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    reduce(a * d - c * b, b * d)
  }
  def times(x: Element[R], y: Element[R]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    reduce(a * c, b * d)
  }
  def divide(x: Element[R], y: Element[R]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    reduce(a * d, b * c)
  }
  override def negate(x: Element[R]) = {
    val Element(n, d) = x
    apply(-n, d)
  }
  def compare(x: Element[R], y: Element[R]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    val s = ring.compare(a, c)
    if (s < 0) -1
    else if (s > 0) 1
    else ring.compare(b, d)
  }
  override def toCode(x: Element[R], precedence: Int) = {
    val Element(n, d) = x
    if (ring.isOne(d)) ring.toCode(n, precedence)
    else {
      val s = ring.toCode(n, 2) + "/" + ring.toCode(d, 2)
      val fenced = precedence > 1
      if (fenced) "(" + s + ")" else s
    }
  }
}

object Quotient {
  case class Element[R](_1: R, _2: R)(val factory: Quotient[R]) extends Product2[R, R] with UniqueFactorizationDomain.Element[Element[R]]
  implicit def ring2quotient[S <% R, R: Quotient](value: S) = implicitly[Quotient[R]].apply(value)
}
