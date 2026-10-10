package scas.residue

import scas.structure.BooleanRing
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialOverUFD
import scas.polynomial.tree.PolynomialWithGB
import scas.power.Lexicographic
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.base.{BigInteger, Boolean}
import BigInteger.given

class BooleanAlgebra(using PolynomialOverUFD[Element[Boolean, Array[Int]], Boolean, Array[Int]]) extends BooleanAlgebra.Impl {
  def this(variables: Variable*) = this(using new PolynomialWithGB(using Boolean, new Lexicographic[Int](variables*)))
}

object BooleanAlgebra {
  def apply[S : Conversion[Variable]](s: S*) = new Conv(s.map(~_)*)

  trait Impl extends Residue[Element[Boolean, Array[Int]], Boolean, Array[Int]] with BooleanRing[Element[Boolean, Array[Int]]] {
    init
    def init: Unit = {
      update(generators.map(_.defining)*)
    }
    extension (x: Element[Boolean, Array[Int]]) {
      def defining = x+x\2
      override def toCode(level: Level) = ring.toCode(x)(level, " ^ ", " && ")
      override def toMathML = ring.toMathML(x)("xor", "and")
    }
  }

  class Conv(variables: Variable*) extends BooleanAlgebra(variables*) with UniqueFactorizationDomain.Conv[Element[Boolean, Array[Int]]] with BooleanRing.Conv[Element[Boolean, Array[Int]]] {
    given instance: Conv = this
  }
}
