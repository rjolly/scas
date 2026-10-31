package scas.polynomial.gb

import scas.power.{PowerProduct, POT}
import scas.polynomial.PolynomialWithSignature
import scas.base.{BigInteger, Boolean}
import Boolean.self.given
import scas.math.Ordering
import SignatureEngine.Pair

class SignatureEngine[T, C, M : PowerProduct](fussy: Boolean, val tail: Boolean)(using factory: PolynomialWithSignature[T, C, M]) extends GMEngine.Impl[PolynomialWithSignature.Element[T, M], C, M, Pair[M]] {
  def this(fussy: Boolean, tail: Boolean)(factory: PolynomialWithSignature[T, C, M]) = this(fussy, tail)(using factory.pp, factory)
  import factory.module
  import module.{max, pp2pot}

  override def ordering = Ordering by { pair => (pair.s, pair.scm, pair.j, pair.i) }

  override def natural = if fussy then ordering else super.natural

  extension (p1: Pair[M]) override def | (p2: Pair[M]) = super.|(p1)(p2) && (fussy >> (p1 < p2))

  def apply(i: Int, j: Int, reduction: Boolean, principal: Int, coprime: Boolean, scm: M) = new Pair(i, j, reduction, principal, coprime, scm, max(i.signature * (scm / i.headPowerProduct), j.signature * (scm / j.headPowerProduct)))

  extension (pair: Pair[M]) def show = "{" + pair.i + ", " + pair.j + "}, " + pair.s.show + ", " + pair.reduction

  extension (i: Int) {
    override def show = super.show(i) + ", " + i.signature.show
    def signature = i.poly.signature
  }
}

object SignatureEngine {
  class Pair[M](i: Int, j: Int, reduction: Boolean, principal: Int, coprime: Boolean, scm: M, val s: POT.Element[M]) extends GBEngine.Pair(i, j, reduction, principal, coprime, scm)
}
