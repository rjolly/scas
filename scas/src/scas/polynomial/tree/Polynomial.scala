package scas.polynomial.tree

import scas.structure.Ring
import scas.power.PowerProduct
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import TreePolynomial.Element

class Polynomial[C : Ring, N : Numeric : ClassTag](s: Variable*) extends TreePolynomial[C, N](s*) with scas.structure.conversion.Ring[Element[C, N]] {
  given instance: Polynomial[C, N] = this
  def newInstance(pp: PowerProduct[Array[N]]) = new Polynomial(pp.variables*)
}
