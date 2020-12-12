package scas.power

import scas.math.Numeric
import scas.variable.Variable
import scas.int2powerProduct

object ArrayPowerProduct {
  opaque type Element[N] = Array[N]

  abstract class Factory[N : Numeric : ClassTagArray] extends PowerProduct.Factory[Element[N]] {
    def numeric = Numeric[N]
    override def apply(x: Array[N]) = x
    def dependencyOnVariables(x: Element[N]) = (for (i <- 0 until length if (x(i) > numeric.zero)) yield i).toArray
    extension (x: Element[N]) def toCode(level: Level) = {
      var s = "1"
      var m = 0
      for (i <- 0 until length) if (x(i) > numeric.zero) {
        val a = variables(i)
        val b = x(i)
        val t = if (b >< numeric.one) a.toString else s"$a\\$b"
        s = if (m == 0) t else s + "*" + t
        m += 1
      }
      s
    }
    extension (x: Element[N]) def toMathML = {
      var s = "<cn>1</cn>"
      var m = 0
      for (i <- 0 until length) if (x(i) > numeric.zero) {
        val a = variables(i)
        val b = x(i)
        val t = if (b >< numeric.one) a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
        s = if (m == 0) t else s"<apply><times/>$s$t</apply>"
        m += 1
      }
      s
    }
    def size(x: Element[N]) = {
      var m = 0
      for (i <- 0 until length) if (x(i) > numeric.zero) m += 1
      m
    }
    extension (x: Element[N]) def apply(i: Int) = x(i)
    extension (x: Element[N]) def len = x.length
  }
}
