package scas.scripting

import Factors.Element

class NormalForm(conj: Boolean)(using ring: BooleanAlgebra) extends NormalForm.Impl(conj) {
  extension (x: Element[BA, Int]) {
    def isImpl = !conj && x.size == 2 && x.head._1.isNot && !x.last._1.isNot
    def isRevImpl = !conj && x.size == 2 && x.last._1.isNot && !x.head._1.isNot
    override def toCode(level: Level) = if x.isImpl then {
      (!x.head._1).toCode(Level.Power) + " >> " + x.last._1.toCode(Level.Power)
    } else if x.isRevImpl then {
      (!x.last._1).toCode(Level.Power) + " >> " + x.head._1.toCode(Level.Power)
    } else super.toCode(x)(level)
    override def toMathML = if x.isImpl then {
      s"<apply><implies/>${(!x.head._1).toMathML}${x.last._1.toMathML}</apply>"
    } else if x.isRevImpl then {
      s"<apply><implies/>${(!x.last._1).toMathML}${x.head._1.toMathML}</apply>"
    } else super.toMathML(x)
  }
}

object NormalForm {
  class Impl(conj: Boolean)(using val ring: BooleanAlgebra) extends Factors[BA, Int] {
    def empty = Map.empty[BA, Int]
    override def apply(x: BA) = if x.isZero then zero else {
      val s = ring.gb(flip(x)).foldLeft(one)((l, a) => l * super.apply(flip(a)))
      if s.contains(x) then super.apply(x) else s
    }
    def flip(x: BA) = if conj then !x else x
    extension (x: Element[BA, Int]) {
      override def toCode(level: Level) = {
        val p = if x.size == 1 then level else if conj then Level.Multiplication else Level.Addition
        var s = ring.one.show
        var m = 0
        for (a, b) <- x do {
          val t = a.toCode(p)
          val times = if conj then " && " else " || "
          s = if m == 0 then t else s + times + t
          m += 1
        }
        if level > p then fenced(s) else s
      }
      override def toMathML = {
        var s = ring.one.toMathML
        var m = 0
        for (a, b) <- x do {
          val t = a.toMathML
          val times = if conj then "and" else "or"
          s = if m == 0 then t else s"<apply><${times}/>$s$t</apply>"
          m += 1
        }
        s
      }
    }
  }
}
