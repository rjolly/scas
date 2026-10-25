package scas.power

import scala.compiletime.deferred
import scas.base.BigInteger.given

trait Combined[M1, M2] extends PowerProduct[(M1, M2)] {
  given factory1: PowerProduct[M1] = deferred
  given factory2: PowerProduct[M2] = deferred
  val one = (factory1.one, factory2.one)
  def variables = factory1.variables ++ factory2.variables
  def generator(n: Int) = if n < factory1.nbvars then (factory1.generator(n), factory2.one) else (factory1.one, factory2.generator(n - factory1.nbvars))
  def compare(x: (M1, M2), y: (M1, M2)) = {
    val (a, b) = x
    val (c, d) = y
    val s = factory2.compare(b, d)
    if s == 0 then factory1.compare(a, c) else s
  }
  def gcd(x: (M1, M2), y: (M1, M2)) = {
    val (a, b) = x
    val (c, d) = y
    (factory1.gcd(a, c), factory2.gcd(b, d))
  }
  def lcm(x: (M1, M2), y: (M1, M2)) = {
    val (a, b) = x
    val (c, d) = y
    (factory1.lcm(a, c), factory2.lcm(b, d))
  }
  extension (x: (M1, M2)) {
    def degree = {
      val (a, b) = x
      a.degree + b.degree
    }
    def multiply(y: (M1, M2)) = {
      val (a, b) = x
      val (c, d) = y
      (a * c, b * d)
    }
    def divide(y: (M1, M2)) = {
      val (a, b) = x
      val (c, d) = y
      (a / c, b / d)
    }
    def factorOf(y: (M1, M2)) = {
      val (a, b) = x
      val (c, d) = y
      (a | c) && (b | d)
    }
    def dependencyOnVariables = {
      val (a, b) = x
      a.dependencyOnVariables ++ b.dependencyOnVariables.map(_ + factory1.nbvars)
    }
    def projection(n: Int, m: Int) = {
      val (a, b) = x
      (a.projection(n, m), b.projection(n - factory1.nbvars, m - factory1.nbvars))
    }
    def size = {
      val (a, b) = x
      a.size + b.size
    }
    def toCode(level: Level, times: String) = {
      val (a, b) = x
      if a.isOne then b.toCode(level, times) else {
        if b.isOne then a.toCode(level, times) else s"${a.toCode(Level.Multiplication, times)}$times${b.toCode(Level.Multiplication, times)}"
      }
    }
    def toMathML(times: String) = {
      val (a, b) = x
      if a.isOne then b.toMathML(times) else {
        if b.isOne then a.toMathML(times) else s"<apply><$times/>${a.toMathML}${b.toMathML}</apply>"
      }
    }
  }
}
