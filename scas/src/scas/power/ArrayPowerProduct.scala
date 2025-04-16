package scas.power

import scas.math.Numeric
import scas.variable.Variable

trait ArrayPowerProduct[N : Numeric as numeric] extends PowerProduct[Array[N]] {
  def empty: Array[N]
  def newInstance(variables: Variable*): ArrayPowerProduct[N]
  extension (x: Array[N]) def dependencyOnVariables = (for (i <- 0 until length if (x(i) > numeric.zero)) yield i).toArray
  extension (x: Array[N]) def toCode(level: Level, times: String) = {
    var s = "1"
    var m = 0
    for (i <- 0 until length) if (x(i) > numeric.zero) {
      val a = variables(i)
      val b = x(i)
      val t = if (b >< numeric.one) a.toString else s"$a\\$b"
      s = if (m == 0) t else s + times + t
      m += 1
    }
    s
  }
  extension (x: Array[N]) def toMathML(times: String) = {
    var s = "<cn>1</cn>"
    var m = 0
    for (i <- 0 until length) if (x(i) > numeric.zero) {
      val a = variables(i)
      val b = x(i)
      val t = if (b >< numeric.one) a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
      s = if (m == 0) t else s"<apply><$times/>$s$t</apply>"
      m += 1
    }
    s
  }
  extension (x: Array[N]) def size = {
    var m = 0
    for (i <- 0 until length) if (x(i) > numeric.zero) m += 1
    m
  }
}
