package scas.scripting

import scas.residue.BooleanAlgebra
import Factors.Element

class NormalForm(recurse: Boolean, conj: Boolean)(using ring: BooleanAlgebra) extends Factors[BA, Int] {
  def empty = Map.empty[BA, Int]
  def apply(x: BA) = if (x.isZero) zero else ring.gb(if (conj) !x else x).foldLeft(empty)((l, a) => l%* (if (conj) !a else a))
  extension (x: Element[BA, Int]) {
    override def toCode(level: Level) = {
      val p = if (x.size == 1) level else if(conj) Level.Multiplication else Level.Addition
      var s = ring.one.show
      var m = 0
      for ((a, b) <- x) {
        val t = if (!a.isNot || recurse) a.toCode(p) else s"!${(!a).toCode(Level.Power)}"
        val times = if(conj) " && " else " || "
        s = if (m == 0) t else s + times + t
        m += 1
      }
      if (level > p) fenced(s) else s
    }
    override def toMathML = {
      var s = ring.one.toMathML
      var m = 0
      for ((a, b) <- x) {
        val t = if (!a.isNot || recurse) a.toMathML else s"<apply><not/>${(!a).toMathML}</apply>"
        val times = if(conj) "and" else "or"
        s = if (m == 0) t else s"<apply><${times}/>$s$t</apply>"
        m += 1
      }
      s
    }
  }
}
