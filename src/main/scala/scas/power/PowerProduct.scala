package scas.power

import scala.reflect.ClassTag
import scas.{Variable, BigInteger, Function, long2bigInteger}
import scas.structure.ordered.Monoid
import scas.math.{Ordering, Numeric}
import spire.macros.Ops
import scas.Implicits.infixPowerProductOps
import Ordering.Implicits.infixOrderingOps
import Numeric.Implicits.infixNumericOps
import Function.identity

trait PowerProduct[@specialized(Byte, Short, Int, Long) N] extends Monoid[Array[N]] {
  implicit def self: PowerProduct[N]
  val variables: Array[Variable]
  implicit val nm: Numeric[N]
  implicit val cm: ClassTag[Array[N]]
  import nm.{fromInt, toLong}
  import variables.length
  def generator(variable: Variable): Array[N] = generator(variables.indexOf(variable))
  def generator(n: Int): Array[N]
  def generators = (for (i <- 0 until length) yield generator(i)).toArray
  def degree(x: Array[N]): Long
  def fromBigInteger(value: BigInteger) = {
    (fromInt(0) /: value.toByteArray()) { (s, b) => s * fromInt(0x100) + fromInt(b & 0xff) }
  }
  def apply(l: Long) = {
    assert (l == 1)
    one
  }
  def random(numbits: Int)(implicit rnd: java.util.Random) = one
  def gcd(x: Array[N], y: Array[N]): Array[N]
  def scm(x: Array[N], y: Array[N]): Array[N]
  def coprime(x: Array[N], y: Array[N]) = gcd(x, y).isOne
  def times(x: Array[N], y: Array[N]): Array[N]
  def divide(x: Array[N], y: Array[N]): Array[N]
  def factorOf(x: Array[N], y: Array[N]): Boolean
  def isUnit(x: Array[N]) = x.isOne
  def dependencyOnVariables(x: Array[N]) = (for (i <- 0 until length if (get(x, i) > fromInt(0))) yield i).toArray
  def projection(x: Array[N], n: Int): Array[N]
  override def toCode(x: Array[N], precedence: Int) = {
    var s = "1"
    var m = 0
    for (i <- 0 until length) if (get(x, i) > fromInt(0)) {
      val t = {
        if (get(x, i) equiv fromInt(1)) variables(i).toString
        else "pow(" + variables(i).toString + ", " + get(x, i) + ")"
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
    for (i <- 0 until length) if (get(x, i) > fromInt(0)) {
      val t = {
        if (get(x, i) equiv fromInt(1)) variables(i).toMathML
        else <apply><power/>{variables(i).toMathML}<cn>{get(x, i)}</cn></apply>
      }
      s = if (m == 0) t else <apply><times/>{s}{t}</apply>
      m += 1
    }
    s
  }
  def toMathML = <list>{variables.map(_.toMathML)}</list>
  def function(x: Array[N], a: Variable) = Function.pow(identity, if (variables.contains(a)) toLong(x(variables.indexOf(a))) else 0)

  def converter(from: Array[Variable]): Array[N] => Array[N]

  def size(x: Array[N]) = {
    var m = 0
    for (i <- 0 until length) if (get(x, i) > fromInt(0)) {
      m += 1
    }
    m
  }

  def get(x: Array[N], i: Int): N
}

object PowerProduct {
  trait ExtraImplicits extends Monoid.ExtraImplicits {
    implicit def infixPowerProductOps[N: PowerProduct](lhs: Array[N]) = new Ops(lhs)
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
