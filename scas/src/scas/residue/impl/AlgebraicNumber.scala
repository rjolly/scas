package scas.residue.impl

import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.structure.commutative.impl.{Residue, Field}
import scas.polynomial.impl.UnivariatePolynomial

class AlgebraicNumber[T, C, M](using ring: UnivariatePolynomial[T, C, M]) extends Residue[T] with Field[T] {
  var list = List.empty[T]
  def generator(n: Int) = ring.generator(n)
  def generators = ring.generators
  def update(mod: T): Unit = {
    // assert mod is irreducible
    list = List(mod)
  }
  def apply(x: T) = x.reduce(list)
  def characteristic = ring.characteristic
  def inverse(x: T) = x.modInverse(list(0))
  override def toString = s"${ring}(${list.map(_.show).mkString(", ")})"
  def toMathML = s"<msub>${ring.toMathML}<list>${list.map(_.toMathML).mkString}</list></msub>"

  extension (ring: Field[C]) def apply(s: T*) = {
    assert (s == this.ring.generators.toList)
    this.ring
  }
  extension (ring: UniqueFactorizationDomain[T]) def apply(s: T*) = {
    assert (s.size == 1 && s(0) >< list(0))
    this
  }
}
