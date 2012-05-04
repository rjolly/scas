package scas.polynomial

import scas.Variable
import scas.structure.{Residue, Field, UniqueFactorizationDomain}
import AlgebraicNumber.Element

class AlgebraicNumber[R <: UnivariatePolynomial.Element[R, C, N], C, @specialized(Int, Long) N](val ring: UnivariatePolynomial[R, C, N]) extends Residue[Element[R, C, N], R] with Field[Element[R, C, N]] with (C => Element[R, C, N]) {
  var mod = ring.zero
  def update(mod: Element[R, C, N]): Unit = update(toRing(mod))
  def update(mod: R) = {
    // assert mod is irreducible
    this.mod = mod
  }
  def generators = ring.generators.map(fromRing)
  def apply(value: C) = fromRing(ring(value))
  def fromRing(x: R) = new Element[R, C, N](if (mod.isZero) x else ring.remainder(x, mod))(this)
  def toRing(x: Element[R, C, N]) = x.value
  def characteristic = ring.characteristic
  def divide(x: Element[R, C, N], y: Element[R, C, N]) = x * inverse(y)
  override def inverse(x: Element[R, C, N]) = fromRing(ring.modInverse(toRing(x), mod))
  override def toString = ring.ring.toString + "(" + mod.toString + ")"
  def toMathML = <msub>{ring.ring.toMathML}{mod.toMathML}</msub>
}

object AlgebraicNumber {
  def apply[C](ring: Field[C], s: Variable): AlgebraicNumber[tree.UnivariatePolynomial.Element[C, Int], C, Int] = apply(tree.UnivariatePolynomial(ring, s))
  def apply[R <: UnivariatePolynomial.Element[R, C, N], C, @specialized(Int, Long) N](ring: UnivariatePolynomial[R, C, N]) = new AlgebraicNumber(ring)

  class Element[R <: UnivariatePolynomial.Element[R, C, N], C, @specialized(Int, Long) N](val value: R)(val factory: AlgebraicNumber[R, C, N]) extends Residue.Element[Element[R, C, N], R] with UniqueFactorizationDomain.Element[Element[R, C, N]]
  implicit def coef2algebraicNumber[D, R <: UnivariatePolynomial.Element[R, C, N], C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: AlgebraicNumber[R, C, N]) = factory(value)
}
