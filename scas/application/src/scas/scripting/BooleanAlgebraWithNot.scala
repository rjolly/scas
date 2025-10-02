package scas.scripting

import scas.variable.Variable

open class BooleanAlgebraWithNot(s: Variable*) extends scas.residue.growable.BooleanAlgebra(s*) {
  extension (x: BA) {
    def isNot = x.coefOne.isOne && !x.isOne
    override def toCode(level: Level) = if !x.isNot then super.toCode(x)(level) else s"!${super.toCode(!x)(Level.Power)}"
    override def toMathML = if !x.isNot then super.toMathML(x) else s"<apply><not/>${super.toMathML(!x)}</apply>"
  }
}
