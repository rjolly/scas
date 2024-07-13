package scas.polynomial.tree

import scas.structure.Ring
import scas.power.{PowerProduct, Lexicographic}
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable

abstract class TreePolynomial[C : Ring, N : Numeric : ClassTag](s: Variable*) extends scas.polynomial.TreePolynomial[C, Array[N]] {
  override given ring: Ring[C] = summon
  override given pp: PowerProduct[Array[N]] = Lexicographic[N](s*)
}

object TreePolynomial {
  type Element[C, N] =  scas.polynomial.TreePolynomial.Element[C, Array[N]]
}
