package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.conversion.UniqueFactorizationDomain
import scas.structure.commutative.Field
import scas.polynomial.TreePolynomial
import scas.variable.Variable
import Variable.string2variable
import TreePolynomial.Element

class UnivariatePolynomial[C](using Field[C])(s: Variable*) extends TreePolynomial[C, Array[Int]] with scas.polynomial.UnivariatePolynomial[Element[C, Array[Int]], C, Array[Int]] with UniqueFactorizationDomain[Element[C, Array[Int]]] {
  override given pp: PowerProduct[Array[Int]] = Lexicographic[Int](s*)
  given instance: UnivariatePolynomial[C] = this
}

object UnivariatePolynomial {
  def apply[C](ring: Field[C])(s: String*) = new UnivariatePolynomial(using ring)(s.map(string2variable)*)
}
