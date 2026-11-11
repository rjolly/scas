package scas.scripting

import scas.power.growable.Lexicographic
import scas.polynomial.tree.growable.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable
import scas.base.Rational

type UPoly = Element[Rational, Array[Int]]

object UPoly {
  def apply(variable: Variable) = new UnivariatePolynomial(0)(using Rational)(variable)
}
