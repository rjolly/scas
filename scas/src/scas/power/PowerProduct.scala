package scas.power

import scas.math.Numeric
import scas.int2powerProduct

abstract class PowerProduct[N : Numeric : ClassTagArray] extends Monoid[Array[N]] {
  def variables: Array[String]
  given PowerProduct[N] = this
  def length = variables.length
  def generator(variable: String): Array[N] = generator(variables.indexOf(variable))
  def generator(n: Int): Array[N]
  def generators = (for (i <- 0 until length) yield generator(i)).toArray
  def degree(x: Array[N]): Long
  def apply(x: Int) = {
    assert (x == 1)
    one
  }
  def gcd(x: Array[N], y: Array[N]): Array[N]
  def scm(x: Array[N], y: Array[N]): Array[N]
  def coprime(x: Array[N], y: Array[N]) = gcd(x, y) >< 1
  def (x: Array[N]) / (y: Array[N]): Array[N]
  def (x: Array[N]) | (y: Array[N]): Boolean
  def (x: Array[N]).isUnit = x >< 1
  def dependencyOnVariables(x: Array[N]) = (for (i <- 0 until length if (x.get(i) > Numeric[N].zero)) yield i).toArray
  def (x: Array[N]).projection(n: Int): Array[N]
  def (x: Array[N]).toCode(level: Level) = {
    var s = "1"
    var m = 0
    for (i <- 0 until length) if (x.get(i) > Numeric[N].zero) {
      val a = variables(i)
      val b = x.get(i)
      val t = if (b >< Numeric[N].one) a else s"$a\\$b"
      s = if (m == 0) t else s"$s*$t"
      m += 1
    }
    s
  }
  def (x: Array[N]).toMathML = {
    var s = "<cn>1</cn>"
    var m = 0
    for (i <- 0 until length) if (x.get(i) > Numeric[N].zero) {
      val a = variables(i)
      val b = x.get(i)
      val t = if (b >< Numeric[N].one) s"<ci>$a</ci>" else s"<apply><power/><ci>$a</ci><cn>$b</cn></apply>"
      s = if (m == 0) t else s"<apply><times/>$s$t</apply>"
      m += 1
    }
    s
  }
  override def apply(x: Array[N]) = x.convert(variables)
  def (x: Array[N]).convert(from: Array[String]): Array[N]
  def size(x: Array[N]) = {
    var m = 0
    for (i <- 0 until length) if (x.get(i) > Numeric[N].zero) {
      m += 1
    }
    m
  }
  def (x: Array[N]).get(i: Int): N
}

object PowerProduct {
  def apply[N : PowerProduct] = summon[PowerProduct[N]]
}
