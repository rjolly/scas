package scas.scripting

import scas.residue.BooleanAlgebra

class NormalForm(using val ring: BooleanAlgebra) extends Factors[BA, Int] {
  def empty = Map.empty[BA, Int]
  override def apply(x: BA) = if (x.isZero) zero else ring.gb(x).foldLeft(one)((l, a) => if (!a.isZero) l * super.apply(a) else l)
  extension (x: NF) override def toCode(level: Level) = {
    var s = ring.one.show
    var m = 0
    for ((a, b) <- x) {
      val t = if (!a.isNot) a.show else s"!${(!a).toCode(Level.Multiplication)}"
      s = if (m == 0) t else s + " || " + t
      m += 1
    }
    s
  }
  extension (x: NF) override def toMathML = {
    var s = ring.one.toMathML
    var m = 0
    for ((a, b) <- x) {
      val t = if (!a.isNot) a.toMathML else s"<apply><not/>${(!a).toMathML}</apply>"
      s = if (m == 0) t else s"<apply><or/>$s$t</apply>"
      m += 1
    }
    s
  }
}
