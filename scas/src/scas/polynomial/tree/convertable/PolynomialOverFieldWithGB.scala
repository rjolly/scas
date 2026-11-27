package scas.polynomial.tree.convertable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.splitable.ArrayPowerProduct
import scas.structure.commutative.{Field, UniqueFactorizationDomain}
import scas.polynomial.TreePolynomial
import scas.polynomial.tree.PolynomialWithGB.Element

class PolynomialOverFieldWithGB[C, N : {Numeric, ClassTag}](using Field[C], ArrayPowerProduct[N]) extends TreePolynomial[C, Array[N]] with scas.polynomial.ufd.convertable.PolynomialOverFieldWithGB[Element[C], C, N] with UniqueFactorizationDomain.Conv[Element[C][Array[N]]] {
  def this(ring: Field[C], pp: ArrayPowerProduct[N]) = this(using summon, summon, ring, pp)
  given instance: PolynomialOverFieldWithGB[C, N] = this
  def newInstance = [M] => (pp) => new scas.polynomial.tree.PolynomialOverFieldWithGB(using ring, pp)
  def newInstance(pp: ArrayPowerProduct[N]) = new PolynomialOverFieldWithGB(ring, pp)
}
