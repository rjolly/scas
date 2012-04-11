package scas.polynomial

import scas.{Variable, int2powerProduct}
import scas.polynomial.ordering.Ordering
import scas.structure.Monoid

class PowerProduct[@specialized(Int, Long) N](val variables: Array[Variable])(implicit nm: Numeric[N], m: Manifest[N], cm: ClassManifest[Array[N]], val ordering: Ordering[N]) extends Monoid[Array[N]] {
  import scala.math.Ordering.Implicits.infixOrderingOps
  import Numeric.Implicits.infixNumericOps
  import nm.{fromInt, toLong}
  def take(n: Int) = new PowerProduct[N](variables.take(n))
  def drop(n: Int) = new PowerProduct[N](variables.drop(n))
  override def one = new Array[N](length + 1)
  def generator(n: Int) = (for (i <- 0 until length + 1) yield fromInt(if (i == n || i == length) 1 else 0)).toArray
  def generators = (for (i <- 0 until length) yield generator(i)).toArray
  def generatorsBy(n: Int) = {
    val m = length/n
    (for (i <- 0 until m) yield (for (j <- 0 until n) yield generator(i * n + j)).toArray).toArray
  }
  def degree(x: Array[N]) = toLong(x(length))
  override def pow(x: Array[N], exp: java.math.BigInteger) = {
    assert (exp.signum() >= 0)
    val n = fromBigInteger(exp)
    (for (i <- 0 until x.length) yield x(i) * n).toArray
  }
  def fromBigInteger(value: java.math.BigInteger) = {
    (fromInt(0) /: value.toByteArray()) { (s, b) => s * fromInt(0xff) + fromInt(b) }
  }
  def apply(l: Long) = {
    assert (l == 1)
    one
  }
  def apply(x: Array[N]) = x
  def random(numbits: Int)(implicit rnd: java.util.Random) = one
  def gcd(x: Array[N], y: Array[N]): Array[N] = {
    val r = new Array[N](length + 1)
    for (i <- 0 until length) r(i) = nm.min(x(i), y(i))
    r(length) = (fromInt(0) /: r) { (s, l) => s + l }
    r
  }
  def scm(x: Array[N], y: Array[N]): Array[N] = {
    val r = new Array[N](length + 1)
    for (i <- 0 until length) r(i) = nm.max(x(i), y(i))
    r(length) = (fromInt(0) /: r) { (s, l) => s + l }
    r
  }
  def compare(x: Array[N], y: Array[N]) = ordering.compare(x, y)
  def times(x: Array[N], y: Array[N]) = (for (i <- 0 until x.length) yield x(i) + y(i)).toArray
  def divide(x: Array[N], y: Array[N]) = (for (i <- 0 until x.length) yield {
    assert (x(i) >= y(i))
    x(i) - y(i)
  }).toArray
  def factorOf(x: Array[N], y: Array[N]): Boolean = {
    for (i <- 0 until x.length) if (x(i) > y(i)) return false
    true
  }
  def isUnit(x: Array[N]) = x.isOne
  override def isOne(x: Array[N]) = x(length) equiv fromInt(0)
  def dependencyOnVariables(x: Array[N]) = (for (i <- 0 until length if (x(i) > fromInt(0))) yield i).toArray
  def projection(x: Array[N], n: Int) = (for (i <- 0 until x.length) yield if (i == n || i == length) x(n) else fromInt(0)).toArray
  override def toCode(x: Array[N], precedence: Int) = {
    var s = ""
    var m = 0
    for (i <- 0 until length) if (x(i) > fromInt(0)) {
      val t = {
        if (x(i) equiv fromInt(1)) variables(i).toString
        else "pow(" + variables(i) + ", " + x(i) + ")"
      }
      s = s + (if (m == 0) "" else "*") + t
      m += 1
    }
    if (m == 0) "1" else s
  }
  override def toString = "["+variables.mkString(", ")+"]"

  def converter(from: Array[Variable]): Array[N] => Array[N] = { x =>
    val r = new Array[N](length + 1)
    val index = from map { a => variables.indexWhere(_ equiv a) }
    for (i <- 0 until from.length if (x(i) > fromInt(0))) {
      val c = index(i)
      assert (c > -1)
      r(c) = x(i)
    }
    r(length) = x(from.length)
    r
  }

  def length = variables.length

  def size(x: Array[N]) = {
    var m = 0
    for (i <- 0 until length) if (x(i) > fromInt(0)) {
      m += 1
    }
    m
  }

  class Ops(val lhs: Array[N]) extends super[Monoid].Ops {
    def /(rhs: Array[N]) = divide(lhs, rhs)
    def |(rhs: Array[N]) = factorOf(lhs, rhs)
  }
  override implicit def mkOps(lhs: Array[N]): Ops = new Ops(lhs)
}

object PowerProduct {
  trait ExtraImplicits {
    implicit def infixPowerProductOps[N: PowerProduct](lhs: Array[N]) = implicitly[PowerProduct[N]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  def apply[@specialized(Int, Long) N](variables: Array[Variable])(implicit nm: Numeric[N], m: Manifest[N], cm: ClassManifest[Array[N]], ordering: Ordering[N]) = new PowerProduct[N](variables)
  def apply[@specialized(Int, Long) N](s: Variable)(implicit nm: Numeric[N], m: Manifest[N], cm: ClassManifest[Array[N]], ordering: Ordering[N]) = new PowerProduct[N](Array(s))
  def apply[@specialized(Int, Long) N](s: Variable, ss: Variable*)(implicit nm: Numeric[N], m: Manifest[N], cm: ClassManifest[Array[N]], ordering: Ordering[N]) = new PowerProduct[N](Array(s) ++ ss)
  def apply[@specialized(Int, Long) N](sss: Array[Array[Variable]])(implicit nm: Numeric[N], m: Manifest[N], cm: ClassManifest[Array[N]], ordering: Ordering[N]) = new PowerProduct[N](for (ss <- sss ; s <- ss) yield s)
}
