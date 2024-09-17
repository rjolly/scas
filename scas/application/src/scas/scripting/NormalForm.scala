package scas.scripting

import scas.residue.BooleanAlgebra
import Factors.Element

class NormalForm(conj: Boolean)(using val ring: BooleanAlgebra) extends Factors[BA, Int] {
  def empty = Map.empty[BA, Int]
  override def apply(x: BA) = if (x.isZero) zero else ring.gb(if (conj) !x else x).foldLeft(one)((l, a) => l * super.apply(if (conj) !a else a))
  extension (x: Element[BA, Int]) {
    override def toCode(level: Level) = {
      var s = ring.one.show
      var m = 0
      for ((a, b) <- x) {
        val t = if (!a.isNot || !conj) a.show else s"!${(!a).toCode(Level.Multiplication)}"
        val times = if(conj) " && " else " || "
        s = if (m == 0) t else s + times + t
        m += 1
      }
      s
    }
    override def toMathML = {
      var s = ring.one.toMathML
      var m = 0
      for ((a, b) <- x) {
        val t = if (!a.isNot || !conj) a.toMathML else s"<apply><not/>${(!a).toMathML}</apply>"
        val times = if(conj) "and" else "or"
        s = if (m == 0) t else s"<apply><${times}/>$s$t</apply>"
        m += 1
      }
      s
    }
  }
}
