package scas.polynomial.tree.binary.mutable

import scas.power.POT
import scas.power.compact.BinaryPowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.mutable.TreePolynomial
import scas.polynomial.TreePolynomial.Element

class Polynomial[C : UniqueFactorizationDomain](using BinaryPowerProduct) extends TreePolynomial[C, Array[Int]] with scas.polynomial.ufd.binary.Polynomial[Element[C, Array[Int]], C] with scas.polynomial.binary.mutable.Polynomial[Element[C, Array[Int]], C] with UniqueFactorizationDomain.Conv[Element[C, Array[Int]]] {
  given instance: Polynomial[C] = this
  def newInstance(pp: POT[Int]) = new scas.polynomial.tree.mutable.PolynomialWithGB(using ring, pp)
}
