package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.conversion.BooleanRing
import scas.structure.commutative.conversion.UniqueFactorizationDomain
import scas.structure.commutative.Field
import scas.polynomial.TreePolynomial
import TreePolynomial.Element
import scas.variable.Variable
import scas.base.Boolean
import Boolean.given

class BooleanPolynomial(s: Variable*) extends TreePolynomial[Boolean, Array[Int]] with scas.polynomial.BooleanPolynomial[Element[Boolean, Array[Int]], Array[Int]] with UniqueFactorizationDomain[Element[Boolean, Array[Int]]] with BooleanRing[Element[Boolean, Array[Int]]] {
  override given pp: PowerProduct[Array[Int]] = Lexicographic[Int](s*)
  def newInstance(pp: PowerProduct[Array[Int]]) = new BooleanPolynomial(pp.variables*)
  given instance: BooleanPolynomial = this
}
