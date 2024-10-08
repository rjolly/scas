package scas.residue

import scas.power.Lexicographic
import scas.structure.BooleanRing
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.PolynomialWithGB
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.base.{BigInteger, Boolean}
import BigInteger.given

class BooleanAlgebra(using val ring: PolynomialWithGB[Element[Boolean, Array[Int]], Boolean, Int]) extends Residue[Element[Boolean, Array[Int]], Boolean, Int] with BooleanRing[Element[Boolean, Array[Int]]] {
  def this(s: Variable*) = this(using new scas.polynomial.tree.PolynomialWithGB(using Boolean, Lexicographic(0)(s*)))
  update(generators.map(x => x+x\2)*)
  extension (x: Element[Boolean, Array[Int]]) {
    override def toCode(level: Level) = ring.toCode(x)(level, " ^ ", " && ")
    override def toMathML = ring.toMathML(x)("xor", "and")
  }
}

object BooleanAlgebra {
  def apply[S : Conversion[Variable]](s: S*) = new Conv(s*)

  class Conv[S : Conversion[Variable]](s: S*) extends BooleanAlgebra(s.map(~_)*) with UniqueFactorizationDomain.Conv[Element[Boolean, Array[Int]]] with BooleanRing.Conv[Element[Boolean, Array[Int]]] {
    given instance: Conv[S] = this
  }
}
