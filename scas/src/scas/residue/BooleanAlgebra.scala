package scas.residue

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.structure.BooleanRing
import scas.polynomial.Polynomial
import scas.power.{PowerProduct, Lexicographic}
import scas.power.degree.DegreeReverseLexicographic
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.base.{BigInteger, Boolean}
import BigInteger.given
import BooleanAlgebra.{Conv, Element}

class BooleanAlgebra[N : {Numeric, ClassTag}](using Polynomial[Element[Array[N]], Boolean, Array[N]]) extends Conv[N] {
  def this(pp: PowerProduct[Array[N]]) = this(using summon, summon, new scas.polynomial.tree.mutable.Polynomial(using Boolean, pp))
  def this(degree: N)(variables: Variable*) = this(new Lexicographic(degree)(variables*))
  given instance: BooleanAlgebra[N] = this
}

object BooleanAlgebra {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(s: S*) = new BooleanAlgebra(degree)(s.map(~_)*)
  inline def degreeReverse[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(s: S*) = new BooleanAlgebra(DegreeReverseLexicographic.inlined(degree)(s*))
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

  abstract class Conv[N : {Numeric, ClassTag}](using Polynomial[Element[Array[N]], Boolean, Array[N]]) extends Impl[Array[N]] with BooleanRing.Conv[Element[Array[N]]] {
    def instance: Conv[N]
  }

  type Element[M] = scas.polynomial.TreePolynomial.Element[Boolean, M]
}
