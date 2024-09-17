package scas.scripting

import scas.variable.Variable

class BooleanAlgebraWithNot(s: Variable*) extends scas.residue.BooleanAlgebra(s*) {
  extension (x: BA) {
    def isNot = x.coefOne.isOne && !x.isOne
    override def toCode(level: Level) = if (!x.isNot) super.toCode(x)(level) else s"!${super.toCode(!x)(Level.Power)}"
    override def toMathML = if (!x.isNot) super.toMathML(x) else s"<apply><not/>${super.toMathML(!x)}</apply>"
  }
}
