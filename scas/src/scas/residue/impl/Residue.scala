package scas.residue.impl

import scas.structure.commutative.impl.{Field, UniqueFactorizationDomain}
import scas.polynomial.impl.PolynomialOverField
import scas.variable.Variable

trait Residue[T, C, M](using val ring: PolynomialOverField[T, C, M]) extends scas.structure.commutative.impl.Residue[T] with Field[T] {
  var list = List.empty[T]
  export ring.generators
  def update(mod: T): Unit = {
    // assert mod is irreducible
    list = List(mod)
  }
  import ring.{generator, variables}
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

  extension (ring: UniqueFactorizationDomain[T]) def apply(s: T*) = {
    assert (s.size == 1 && s(0) >< list(0))
    this
  }
}