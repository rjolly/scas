package scas.residue

import scas.structure.BooleanRing
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialOverUFD
import scas.polynomial.tree.PolynomialWithGB
import scas.power.{ArrayPowerProduct, Lexicographic}
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.base.{BigInteger, Boolean}
import BigInteger.given
import BooleanAlgebra.Impl

abstract class BooleanAlgebra(using PolynomialOverUFD[Element[Boolean, Array[Int]], Boolean, Array[Int]]) extends Impl[Array[Int]] with UniqueFactorizationDomain.Conv[Element[Boolean, Array[Int]]] with BooleanRing.Conv[Element[Boolean, Array[Int]]] {
  def this(pp: ArrayPowerProduct[Int]) = this(using new PolynomialWithGB(using Boolean, pp))
  def this(variables: Variable*) = this(new Lexicographic[Int](variables*))
  def instance: BooleanAlgebra
}

object BooleanAlgebra {
  def apply[S : Conversion[Variable]](s: S*) = new Conv(s.map(~_)*)
  def binary[S : Conversion[Variable]](s: S*) = new scas.residue.binary.BooleanAlgebra.Conv(s.map(~_)*)

  trait Impl[M] extends Residue[Element[Boolean, M], Boolean, M] with BooleanRing[Element[Boolean, M]] {
    init
    def init: Unit = {
      update(generators.map(_.defining)*)
    }
    extension (x: Element[Boolean, M]) {
      def defining = x+x\2
      override def toCode(level: Level) = ring.toCode(x)(level, " ^ ", " && ")
      override def toMathML = ring.toMathML(x)("xor", "and")
    }
  }

  class Conv(variables: Variable*) extends BooleanAlgebra(variables*) {
    given instance: BooleanAlgebra = this
  }
}
