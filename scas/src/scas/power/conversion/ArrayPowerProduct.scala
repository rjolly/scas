package scas.power.conversion

import scas.math.Numeric
import scas.variable.Variable

abstract class ArrayPowerProduct[N : Numeric : ClassTagArray] extends PowerProduct[Array[N]] {
  def numeric = Numeric[N]
  def dependencyOnVariables(x: Array[N]) = (for (i <- 0 until length if (x(i) > numeric.zero)) yield i).toArray
  extension (x: Array[N]) def toCode(level: Level) = {
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
  extension (x: Array[N]) def toMathML = {
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
  def size(x: Array[N]) = {
    var m = 0
    for (i <- 0 until length) if (x(i) > numeric.zero) m += 1
    m
  }
}
