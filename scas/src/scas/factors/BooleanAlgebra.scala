package scas.factors

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.residue.BooleanAlgebra.Element
import BooleanAlgebra.Impl

class BooleanAlgebra[N : {Numeric, ClassTag}](degree: N)(on: Boolean, recurse: Boolean, conj: Boolean, s: Variable*) extends scas.residue.growable.BooleanAlgebra(degree)(s*) with Impl[Array[N]] {
  def this(degree: N)(conj: Boolean, s: Variable*) = this(degree)(true, true, conj, s*)
  def this(degree: N)(s: Variable*) = this(degree)(true, s*)
  override def extend(variables: Variable*): Unit = {
    super.extend(variables*)
    if (on) then a.extend(variables*)
  }
  given a: BooleanAlgebra[N] = new BooleanAlgebra(degree)(recurse, false, !conj, s*)
  given nf: NormalForm[Array[N]] = new NormalForm(conj)
  extension (x: Element[Array[N]]) {
    override def toCode(level: Level) = if on then nf(x).toCode(level) else super.toCode(x)(level)
    override def toMathML = if on then nf(x).toMathML else super.toMathML(x)
  }
}

object BooleanAlgebra {
  trait Impl[M] extends scas.residue.growable.BooleanAlgebra.Impl[M] {
    def mod(x: Element[M], xs: Element[M]*) = {
      update(xs.map(!_)*)
      !this(!x)
    }
    extension (x: Element[M]) {
      def isNot = x.coefOne.isOne && !x.isOne
      override def toCode(level: Level) = if !x.isNot then super.toCode(x)(level) else s"!${super.toCode(!x)(Level.Power)}"
      override def toMathML = if !x.isNot then super.toMathML(x) else s"<apply><not/>${super.toMathML(!x)}</apply>"
    }
  }
}
