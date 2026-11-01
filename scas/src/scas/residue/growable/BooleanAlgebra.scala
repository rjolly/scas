package scas.residue.growable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.polynomial.growable.PolynomialWithGB
import scas.power.growable.{ArrayPowerProduct, Lexicographic}
import scas.variable.Variable
import scas.base.{BigInteger, Boolean}
import BigInteger.given
import BooleanAlgebra.Impl
import scas.residue.BooleanAlgebra.Element

open class BooleanAlgebra[N : {Numeric, ClassTag}](using PolynomialWithGB[Element[Array[N]], Boolean, Array[N]]) extends Impl[Array[N]] {
  def this(pp: ArrayPowerProduct[N]) = this(using summon, summon, new scas.polynomial.tree.growable.PolynomialWithGB(using Boolean, pp))
  def this(variables: Variable*) = this(new Lexicographic[N](variables*))
}

object BooleanAlgebra {
  trait Impl[M] extends scas.residue.BooleanAlgebra.Impl[M] with Residue[Element[M], Boolean, M] {
    override def extend(variables: Variable*): Unit = {
      super.extend(variables*)
      update(generators.drop(ring.pp.variables.length - variables.length).map(_.defining)*)
    }
  }
}
