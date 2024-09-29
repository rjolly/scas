package scas.scripting

import Factors.Element

class NormalFormWithImpl(conj: Boolean)(using ring: BooleanAlgebra) extends NormalForm(conj) {
  extension (x: Element[BA, Int]) {
    def isImpl = !conj && x.size == 2 && x.head._1.isNot && !x.last._1.isNot
    def isRevImpl = !conj && x.size == 2 && x.last._1.isNot && !x.head._1.isNot
    override def toCode(level: Level) = if (x.isImpl) {
      (!x.head._1).toCode(Level.Power) + " >> " + x.last._1.toCode(Level.Power)
    } else if (x.isRevImpl) {
      (!x.last._1).toCode(Level.Power) + " >> " + x.head._1.toCode(Level.Power)
    } else super.toCode(x)(level)
    override def toMathML = if (x.isImpl) {
      s"<apply><implies/>${(!x.head._1).toMathML}${x.last._1.toMathML}</apply>"
    } else if (x.isRevImpl) {
      s"<apply><implies/>${(!x.last._1).toMathML}${x.head._1.toMathML}</apply>"
    } else super.toMathML(x)
  }
}
