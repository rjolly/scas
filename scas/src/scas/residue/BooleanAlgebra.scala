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

open class BooleanAlgebra(using_ring: PolynomialWithGB[Element[Boolean, Array[Int]], Boolean, Int]) extends Residue[Element[Boolean, Array[Int]], Boolean, Int] with BooleanRing[Element[Boolean, Array[Int]]] {
  def this(variables: Variable*) = this(new scas.polynomial.tree.PolynomialWithGB(using Boolean, new Lexicographic[Int](variables*)))
  override given ring: () => PolynomialWithGB[Element[Boolean, Array[Int]], Boolean, Int] = using_ring
  update(generators.map(x => x+x\2)*)
  extension (x: Element[Boolean, Array[Int]]) {
    override def toCode(level: Level) = ring.toCode(x)(level, " ^ ", " && ")
    override def toMathML = ring.toMathML(x)("xor", "and")
  }
}

object BooleanAlgebra {
  def apply[S : Conversion[Variable]](s: S*) = new Conv(s.map(~_)*)

  class Conv(variables: Variable*) extends BooleanAlgebra(variables*) with UniqueFactorizationDomain.Conv[Element[Boolean, Array[Int]]] with BooleanRing.Conv[Element[Boolean, Array[Int]]] {
    given instance: Conv = this
  }
}
