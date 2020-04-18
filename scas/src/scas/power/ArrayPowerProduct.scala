package scas.power

import scas.math.Numeric
import scas.variable.Variable
import scas.int2powerProduct

abstract class ArrayPowerProduct[N : Numeric : ClassTagArray] extends PowerProduct[Array[N]] {
  def numeric = Numeric[N]
  def dependencyOnVariables(x: Array[N]) = (for (i <- 0 until length if (x.get(i) > numeric.zero)) yield i).toArray
  def (x: Array[N]).toCode(level: Level) = {
    var s = "1"
    var m = 0
    for (i <- 0 until length) if (x.get(i) > numeric.zero) {
      val a = variables(i)
      val b = x.get(i)
      val t = if (b >< numeric.one) a.toString else s"$a\\$b"
      s = if (m == 0) t else s + "*" + t
      m += 1
    }
    s
  }
  def (x: Array[N]).toMathML = {
    var s = "<cn>1</cn>"
    var m = 0
    for (i <- 0 until length) if (x.get(i) > numeric.zero) {
      val a = variables(i)
      val b = x.get(i)
      val t = if (b >< numeric.one) a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
      s = if (m == 0) t else s"<apply><times/>$s$t</apply>"
      m += 1
    }
    s
  }
  def size(x: Array[N]) = {
    var m = 0
    for (i <- 0 until length) if (x.get(i) > numeric.zero) m += 1
    m
  }
  def (x: Array[N]).get(i: Int): N
}
