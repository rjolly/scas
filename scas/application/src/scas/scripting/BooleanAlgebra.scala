package scas.scripting

import scas.variable.Variable

class BooleanAlgebra(recurse: Boolean, s: Variable*) extends BooleanAlgebraWithNot(s*) {
  def this(s: Variable*) = this(true, s*)
  given nf: NormalForm = new NormalForm(false)(using new BooleanAlgebra(false, s*))
  extension (x: BA) {
    override def toCode(level: Level) = if (recurse) nf(x).toCode(level) else super.toCode(x)(level)
    override def toMathML = if (recurse) nf(x).toMathML else super.toMathML(x)
  }
}
