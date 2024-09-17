package scas.scripting

import scas.variable.Variable

class BooleanAlgebra(recurse: Boolean, conj: Boolean, s: Variable*) extends scas.residue.BooleanAlgebra(s*) {
  def this(s: Variable*) = this(true, false, s*)
  given nf: NormalForm = new NormalForm(conj)(using if (recurse) new BooleanAlgebra(false, true, s*) else scas.residue.BooleanAlgebra(s*))
  extension (x: BA) {
    override def toCode(level: Level) = nf(x).toCode(level)
    override def toMathML = nf(x).toMathML
  }
}
