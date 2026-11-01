package scas.residue

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.structure.{BooleanRing, Ring}
import scas.polynomial.PolynomialWithGB
import scas.power.{PowerProduct, Lexicographic}
import scas.power.degree.DegreeReverseLexicographic
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.base.{BigInteger, Boolean}
import BigInteger.given
import BooleanAlgebra.{Impl, Element}

abstract class BooleanAlgebra[N : {Numeric, ClassTag}](using PolynomialWithGB[Element[Array[N]], Boolean, Array[N]]) extends Impl[Array[N]] with Ring.Conv[Element[Array[N]]] with BooleanRing.Conv[Element[Array[N]]] {
  def this(pp: PowerProduct[Array[N]]) = this(using summon, summon, new scas.polynomial.tree.mutable.PolynomialWithGB(using Boolean, pp))
  def this(variables: Variable*) = this(new Lexicographic[N](variables*))
  def instance: BooleanAlgebra[N]
}

object BooleanAlgebra {
  def apply[N : {Numeric, ClassTag}](ring: PolynomialWithGB[Element[Array[N]], Boolean, Array[N]]) = new Conv2(ring)
  def apply[N : {Numeric, ClassTag}](pp: PowerProduct[Array[N]]) = new Conv1(pp)
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(s: S*) = new Conv[N](s.map(~_)*)
  inline def degreeReverse[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(s: S*) = this(DegreeReverseLexicographic.inlined(degree)(s*))
  def binary = scas.residue.binary.BooleanAlgebra

  trait Impl[M] extends Residue[Element[M], Boolean, M] with BooleanRing[Element[M]] {
    init
    def init: Unit = {
      update(generators.map(_.defining)*)
    }
    extension (x: Element[M]) {
      def defining = x+x\2
      override def toCode(level: Level) = ring.toCode(x)(level, " ^ ", " && ")
      override def toMathML = ring.toMathML(x)("xor", "and")
    }
  }

  class Conv2[N : {Numeric, ClassTag}](ring: PolynomialWithGB[Element[Array[N]], Boolean, Array[N]]) extends BooleanAlgebra(using summon, summon, ring) {
    given instance: BooleanAlgebra[N] = this
  }

  class Conv1[N : {Numeric, ClassTag}](pp: PowerProduct[Array[N]]) extends BooleanAlgebra(pp) {
    given instance: BooleanAlgebra[N] = this
  }

  class Conv[N : {Numeric, ClassTag}](variables: Variable*) extends BooleanAlgebra(variables*) {
    given instance: BooleanAlgebra[N] = this
  }

  type Element[M] = scas.polynomial.TreePolynomial.Element[Boolean, M]
}
