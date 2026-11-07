package scas.residue

import scala.compiletime.deferred
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.polynomial.Polynomial
import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.structure.Ring
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element
import ResidueOverUFD.Conv

class ResidueOverUFD[C, N : {Numeric, ClassTag}](using Polynomial[Element[C, Array[N]], C, Array[N]]) extends Conv[C, N] {
  def this(ring: UniqueFactorizationDomain[C])(pp: PowerProduct[Array[N]]) = this(using summon, summon, new scas.polynomial.tree.mutable.PolynomialWithGB(using ring, pp))
  def this(degree: N)(ring: UniqueFactorizationDomain[C])(variables: Variable*) = this(ring)(new Lexicographic(degree)(variables*))
  given instance: ResidueOverUFD[C, N] = this
}

object ResidueOverUFD {
  trait Impl[T, C, M] extends Residue[T, C, M]

  abstract class Conv[C, N : {Numeric, ClassTag}](using Polynomial[Element[C, Array[N]], C, Array[N]]) extends Impl[Element[C, Array[N]], C, Array[N]] with Ring.Conv[Element[C, Array[N]]] {
    def instance: Conv[C, N]
  }
}
