package scas.scripting

import scas.variable.Variable

class BooleanAlgebra(on: Boolean, recurse: Boolean, conj: Boolean, s: Variable*) extends BooleanAlgebra.WithNot(s*) {
  def this(conj: Boolean, s: Variable*) = this(true, true, conj, s*)
  def this(s: Variable*) = this(true, s*)
  override def extend(variables: Variable*): Unit = {
    super.extend(variables*)
    if (on) then a.extend(variables*)
  }
  given a: BooleanAlgebra = new BooleanAlgebra(recurse, false, !conj, s*)
  given nf: NormalForm = new NormalForm(conj)
  extension (x: BA) {
    override def toCode(level: Level) = if on then nf(x).toCode(level) else super.toCode(x)(level)
    override def toMathML = if on then nf(x).toMathML else super.toMathML(x)
  }
}

object BooleanAlgebra {
  class WithNot(s: Variable*) extends scas.residue.growable.BooleanAlgebra(s*) {
    extension (x: BA) {
      def isNot = x.coefOne.isOne && !x.isOne
      override def toCode(level: Level) = if !x.isNot then super.toCode(x)(level) else s"!${super.toCode(!x)(Level.Power)}"
      override def toMathML = if !x.isNot then super.toMathML(x) else s"<apply><not/>${super.toMathML(!x)}</apply>"
    }
  }
}
