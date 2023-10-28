package scas.residue.impl

import scas.structure.commutative.impl.{Residue, Field, UniqueFactorizationDomain}
import scas.polynomial.impl.UnivariatePolynomial
import scas.variable.Variable

trait AlgebraicNumber[T, C, M] extends Residue[T] with Field[T] {
  val ring: UnivariatePolynomial[T, C, M]
  import ring.given
  var list = List.empty[T]
  export ring.{generator, generators, variables}
  def update(mod: T): Unit = {
    // assert mod is irreducible
    list = List(mod)
  }
  def sqrt(x: T) = {
    val n = variables.indexOf(Variable.sqrt(x))
    assert (n > -1)
    generator(n)
  }
  def apply(x: T) = x.reduce(list)
  def characteristic = ring.characteristic
  def inverse(x: T) = x.modInverse(list(0))
  override def toString = s"${ring}(${list.map(_.show).mkString(", ")})"
  def toMathML = s"<msub>${ring.toMathML}<list>${list.map(_.toMathML).mkString}</list></msub>"

  extension (ring: Field[C]) def apply(s: T*) = this.ring.apply(ring)(s: _*)
  extension (ring: UniqueFactorizationDomain[T]) def apply(s: T*) = {
    assert (s.size == 1 && s(0) >< list(0))
    this
  }
}
