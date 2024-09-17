package scas.scripting

import scas.variable.Variable

class BooleanAlgebra(recurse: Boolean, conj: Boolean, s: Variable*) extends scas.residue.BooleanAlgebra(s*) {
  def this(conj: Boolean, s: Variable*) = this(true, conj, s*)
  def this(s: Variable*) = this(false, s*)
  given nf: NormalForm = new NormalForm(recurse, conj)(using if (recurse) new BooleanAlgebra(false, !conj, s*) else scas.residue.BooleanAlgebra(s*))
  extension (x: BA) {
    override def toCode(level: Level) = nf(x).toCode(level)
    override def toMathML = nf(x).toMathML
  }
}
