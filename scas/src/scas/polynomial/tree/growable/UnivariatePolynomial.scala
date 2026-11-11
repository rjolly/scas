package scas.polynomial.tree.growable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.growable.{PowerProduct, Lexicographic}
import scas.structure.commutative.{UniqueFactorizationDomain, Field}
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class UnivariatePolynomial[C, N : {Numeric, ClassTag}](degree: N)(using Field[C])(variable: Variable) extends TreePolynomial[C, Array[N]] with scas.polynomial.growable.ufd.UnivariatePolynomial[Element[C, Array[N]], C, Array[N]] with UniqueFactorizationDomain.Conv[Element[C, Array[N]]] {
  override given pp: PowerProduct[Array[N]] = Lexicographic(degree)(variable)
  given instance: UnivariatePolynomial[C, N] = this
}

object UnivariatePolynomial {
  def apply[C, N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(ring: Field[C])(s: S) = new UnivariatePolynomial(degree)(using ring)(~s)
}
