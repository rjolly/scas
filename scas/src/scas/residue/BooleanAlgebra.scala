package scas.residue

import scas.structure.{BooleanRing, Ring}
import scas.polynomial.PolynomialWithGB
import scas.power.{PowerProduct, Lexicographic}
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.base.{BigInteger, Boolean}
import BigInteger.given
import BooleanAlgebra.{Impl, Element}

abstract class BooleanAlgebra(using PolynomialWithGB[Element[Array[Int]], Boolean, Array[Int]]) extends Impl[Array[Int]] with Ring.Conv[Element[Array[Int]]] with BooleanRing.Conv[Element[Array[Int]]] {
  def this(pp: PowerProduct[Array[Int]]) = this(using new scas.polynomial.tree.mutable.PolynomialWithGB(using Boolean, pp))
  def this(variables: Variable*) = this(new Lexicographic[Int](variables*))
  def instance: BooleanAlgebra
}

object BooleanAlgebra {
  def apply(pp: PowerProduct[Array[Int]]) = new Conv1(pp)
  def apply[S : Conversion[Variable]](s: S*) = new Conv(s.map(~_)*)
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

  class Conv1(pp: PowerProduct[Array[Int]]) extends BooleanAlgebra(pp) {
    given instance: BooleanAlgebra = this
  }

  class Conv(variables: Variable*) extends BooleanAlgebra(variables*) {
    given instance: BooleanAlgebra = this
  }

  type Element[M] = scas.polynomial.TreePolynomial.Element[Boolean, M]
}
