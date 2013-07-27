package scas.power

import scala.reflect.ClassTag
import scas.{Variable, BigInteger}
import scas.structure.ordered.Monoid
import scas.math.{Ordering, Numeric}
import spire.macros.Ops
import Ordering.Implicits.infixOrderingOps
import Numeric.Implicits.infixNumericOps

trait PowerProduct[@specialized(Byte, Short, Int, Long) N] extends Monoid[Array[N]] {
  val variables: Array[Variable]
  implicit val nm: Numeric[N]
  implicit val m: ClassTag[N]
  implicit val cm: ClassTag[Array[N]]
  import nm.{fromInt, toLong}
  def take(n: Int) = self(variables.take(n))
  def drop(n: Int) = self(variables.drop(n))
  def self(variables: Array[Variable]): PowerProduct[N]
  override def one = new Array[N](length + 1)
  def generator(variable: Variable): Array[N] = generator(variables.indexOf(variable))
  def generator(n: Int) = (for (i <- 0 until length + 1) yield fromInt(if (i == n || i == length) 1 else 0)).toArray
  def generators = (for (i <- 0 until length) yield generator(i)).toArray
  def degree(x: Array[N]) = toLong(x(length))
  override def pow(x: Array[N], exp: BigInteger) = {
    assert (exp.signum() >= 0)
    val n = fromBigInteger(exp)
    (for (i <- 0 until x.length) yield x(i) * n).toArray
  }
  def fromBigInteger(value: BigInteger) = {
    (fromInt(0) /: value.toByteArray()) { (s, b) => s * fromInt(0x100) + fromInt(b & 0xff) }
  }
  def apply(l: Long) = {
    assert (l == 1)
    one
  }
  def convert(x: Array[N]) = x
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
  def coprime(x: Array[N], y: Array[N]) = gcd(x, y).isOne
  def times(x: Array[N], y: Array[N]) = multiply(x.clone, y)
  def multiply(x: Array[N], y: Array[N]) = {
    var i = 0
    while (i < x.length) {
      x(i) += y(i)
      i += 1
    }
    x
  }
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
    var s = "1"
    var m = 0
    for (i <- 0 until length) if (x(i) > fromInt(0)) {
      val t = {
        if (x(i) equiv fromInt(1)) variables(i).toString
        else "pow(" + variables(i).toString + ", " + x(i) + ")"
      }
      s = if (m == 0) t else s + "*" + t
      m += 1
    }
    s
  }
  override def toString = "["+variables.mkString(", ")+"]"
  def toMathML(x: Array[N]) = {
    var s = <cn>1</cn>
    var m = 0
    for (i <- 0 until length) if (x(i) > fromInt(0)) {
      val t = {
        if (x(i) equiv fromInt(1)) variables(i).toMathML
        else <apply><power/>{variables(i).toMathML}<cn>{x(i)}</cn></apply>
      }
      s = if (m == 0) t else <apply><times/>{s}{t}</apply>
      m += 1
    }
    s
  }
  def toMathML = <list>{variables.map(_.toMathML)}</list>

  def converter(from: Array[Variable]): Array[N] => Array[N] = { x =>
    val r = new Array[N](length + 1)
    val index = from map { a => variables.indexOf(a) }
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

  override implicit def mkOps(lhs: Array[N]) = new PowerProduct.Ops(lhs)(this)
}

object PowerProduct {
  trait ExtraImplicits extends Monoid.ExtraImplicits {
    implicit def infixPowerProductOps[N: PowerProduct](lhs: Array[N]) = implicitly[PowerProduct[N]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  def apply(variables: Variable*) = lexicographic[Int](variables: _*)
  def lexicographic[@specialized(Byte, Short, Int, Long) N](variables: Variable*)(implicit nm: Numeric[N], m: ClassTag[N], cm: ClassTag[Array[N]]) = new Lexicographic[N](variables.toArray)
  def degreeLexicographic[@specialized(Byte, Short, Int, Long) N](variables: Variable*)(implicit nm: Numeric[N], m: ClassTag[N], cm: ClassTag[Array[N]]) = new DegreeLexicographic[N](variables.toArray)
  def degreeReverseLexicographic[@specialized(Byte, Short, Int, Long) N](variables: Variable*)(implicit nm: Numeric[N], m: ClassTag[N], cm: ClassTag[Array[N]]) = new DegreeReverseLexicographic[N](variables.toArray)
  def kthElimination[@specialized(Byte, Short, Int, Long) N](variables: Variable*)(k: Int)(implicit nm: Numeric[N], m: ClassTag[N], cm: ClassTag[Array[N]]) = new KthElimination[N](variables.toArray, k)

  class Ops[N: PowerProduct](lhs: Array[N]) extends Monoid.Ops[Array[N]] {
    def /(rhs: Array[N]) = macro Ops.binop[Array[N], Array[N]]
    def |(rhs: Array[N]) = macro Ops.binop[Array[N], Boolean]
  }
}
