package scas.polynomial.tree

import scala.collection.SortedMap
import scas.Variable
import scas.polynomial.{TreePolynomial, PowerProduct, Module}
import scas.structure.Field
import scas.module.Module.{Element => Module_Element}
import AlgebraicNumber.Element

class AlgebraicNumber[C, @specialized(Int, Long) N](val ring: Field[C], val pp: PowerProduct[N], val module: Module[Polynomial.Element[C, N], C, N])(implicit val cm: ClassManifest[Element[C, N]]) extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.AlgebraicNumber[Polynomial.Element, Element[C, N], C, N] {
  def this(ring: Field[C], pp: PowerProduct[N]) = this(ring, pp, Module("e", 2, Polynomial(ring, pp)))
  def apply(x: Element[C, N], element: Module_Element[Polynomial.Element[C, N]]) = apply(x.value, element)
  def apply(value: SortedMap[Array[N], C], element: Module_Element[Polynomial.Element[C, N]]) = new Element(value, element)(this)
  def apply(value: SortedMap[Array[N], C]) = apply(value, module.zero)
  def fromPolynomial(x: Polynomial.Element[C, N]) = apply(x.value)
}

object AlgebraicNumber {
  def apply[C](ring: Field[C], s: Variable): AlgebraicNumber[C, Int] = apply(ring, PowerProduct(s))
  def apply[C, @specialized(Int, Long) N](ring: Field[C], pp: PowerProduct[N]) = new AlgebraicNumber(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C], val element: Module_Element[Polynomial.Element[C, N]])(val factory: AlgebraicNumber[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with scas.polynomial.AlgebraicNumber.Element[Polynomial.Element, Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2algebraicNumber[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: AlgebraicNumber[C, N]) = factory(value)
  }
}
