package scas.power

import scas.math.Numeric
import scas.util.ClassTagArray
import scas.variable.Variable

trait ArrayPowerProduct[N : ClassTagArray](using numeric: Numeric[N]) extends PowerProduct[Array[N]] {
  def dependencyOnVariables(x: Array[N]) = {
    import numeric.>
    (for (i <- 0 until length if (x(i) > numeric.zero)) yield i).toArray
  }
  extension (x: Array[N]) def toCode(level: Level) = {
    import numeric.{>, ><}
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
    import numeric.{>, ><}
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
    import numeric.>
    var m = 0
    for (i <- 0 until length) if (x(i) > numeric.zero) m += 1
    m
  }
}
