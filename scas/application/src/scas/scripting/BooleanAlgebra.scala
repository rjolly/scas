package scas.scripting

import scas.variable.Variable

class BooleanAlgebra(on: Boolean, recurse: Boolean, conj: Boolean, s: Variable*) extends BooleanAlgebraWithNot(s*) {
  def this(conj: Boolean, s: Variable*) = this(true, true, conj, s*)
  def this(s: Variable*) = this(true, s*)
  override def extend(variables: Variable*): Unit = {
    super.extend(variables*)
    if (on) then a.extend(variables*)
  }
  given a: BooleanAlgebra = new BooleanAlgebra(recurse, false, !conj, s*)
  given nf: NormalForm = new NormalFormWithImpl(conj)
  extension (x: BA) {
    override def toCode(level: Level) = if on then nf(x).toCode(level) else super.toCode(x)(level)
    override def toMathML = if on then nf(x).toMathML else super.toMathML(x)
  }
}
