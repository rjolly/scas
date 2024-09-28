package scas.scripting

import scas.variable.Variable

class BooleanAlgebra(on: Boolean, recurse: Boolean, conj: Boolean, s: Variable*) extends BooleanAlgebraWithNot(s*) {
  def this(conj: Boolean, s: Variable*) = this(true, true, conj, s*)
  def this(s: Variable*) = this(true, s*)
  given nf: NormalForm = new NormalForm(conj)(using new BooleanAlgebra(recurse, false, !conj, s*))
  extension (x: BA) {
    override def toCode(level: Level) = if (on) nf(x).toCode(level) else super.toCode(x)(level)
    override def toMathML = if (on) nf(x).toMathML else super.toMathML(x)
  }
}
