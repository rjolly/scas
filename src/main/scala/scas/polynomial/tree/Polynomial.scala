package scas.polynomial.tree

import scala.collection.SortedMap
import scas.Variable
import scas.polynomial.ordering.Ordering
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.Ring
import Polynomial.Element

trait Polynomial[C, @specialized(Int, Long) N] extends TreePolynomial[Element[C, N], C, N] {
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object Polynomial {
  def apply[C](ring: Ring[C], variables: Variable*): Polynomial[C, Int] = apply(ring, PowerProduct(variables.toArray, Ordering.lexicographic[Int]))
  def apply[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N]) = new PolynomialImpl(ring, pp)
  def solvable[C](ring: Ring[C], variables: Variable*): Polynomial[C, Int] = solvable(ring, PowerProduct(variables.toArray, Ordering.lexicographic[Int]))
  def solvable[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N]) = new SolvablePolynomial(ring, pp)
  def weylAlgebra[C](ring: Ring[C], variables: Variable*): Polynomial[C, Int] = weylAlgebra(ring, PowerProduct(variables.toArray, Ordering.lexicographic[Int]))
  def weylAlgebra[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N]) = new WeylAlgebra(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C])(val factory: Polynomial[C, N]) extends TreePolynomial.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: Polynomial[C, N]) = factory(value)
  }
}
