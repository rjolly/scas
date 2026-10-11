package scas.polynomial.tree.binary.growable

import scas.power.POT
import scas.power.compact.growable.BinaryPowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class Polynomial[C : UniqueFactorizationDomain](using BinaryPowerProduct) extends TreePolynomial[C, Array[Int]] with scas.polynomial.ufd.binary.growable.Polynomial[Element[C, Array[Int]], C] with UniqueFactorizationDomain.Conv[Element[C, Array[Int]]] {
  given instance: Polynomial[C] = this
  def newInstance(pp: POT[Int]) = new scas.polynomial.tree.PolynomialWithGB(using ring, pp)
}
