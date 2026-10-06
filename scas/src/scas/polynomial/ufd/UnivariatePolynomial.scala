package scas.polynomial.ufd

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scas.structure.commutative.{EuclidianDomain, Field}
import scas.power.PowerProduct
import scas.polynomial.PolynomialWithRepr
import PolynomialWithRepr.Element
import UnivariatePolynomial.Repr

trait UnivariatePolynomial[T : ClassTag, C, M] extends PolynomialWithModInverse[T, C, M] with EuclidianDomain[T] {
  assert (pp.nbvars == 1)
  def derivative(x: T) = x.map((a, b) => (a / pp.generator(0), b * ring.fromInt(a.degree)))
  override def gcd(x: T, y: T) = gcd1(x, y)
  @tailrec final def gcd1(x: T, y: T): T = if y.isZero then x else gcd1(y, x.reduce(y))
  extension (x: T) {
    override def reduce(m: M, a: C, y: T, b: C, remainder: Boolean) = x.subtract(m, a / b, y)
    override def reduce(ys: T*) = super.reduce(x)(ys*)
  }
  extension (x: T) def modInverse(mods: T*) = {
    assert (mods.length == 1)
    val s = new Repr(using this)(1)
    val (p, e) = s.gcd(s(x, 0), s(mods(0)))
    assert (p.isUnit)
    e(0) / p
  }
}

object UnivariatePolynomial {
  class Repr[T : ClassTag, C, M](using UnivariatePolynomial[T, C, M])(val dimension: Int) extends PolynomialWithRepr[T, C, M] with UnivariatePolynomial[Element[T], C, M] {
    override given factory: UnivariatePolynomial[T, C, M] = summon
    override given ring: Field[C] = factory.ring
    override given pp: PowerProduct[M] = factory.pp
  }
}
