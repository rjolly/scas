package scas.polynomial

import scas.structure.Ring
import scas.polynomial.ordering.Ordering
import scas.Implicits.{infixRingOps, infixPowerProductOps}
import Polynomial.Element

trait Polynomial[T <: Element[T, C, N], C, N] extends Ring[T] {
  implicit val ring: Ring[C]
  implicit val pp: PowerProduct[N]
  implicit val cm: ClassManifest[T]
  def generators = pp.generators.map(fromPowerProduct)
  def generatorsBy(n: Int) = pp.generatorsBy(n).map(_.map(fromPowerProduct))
  def characteristic = ring.characteristic
  def apply(l: Long) = apply(ring(l))
  def random(numbits: Int)(implicit rnd: java.util.Random) = zero
  def compare(x: T, y: T): Int = {
    val it = iterator(y)
    for ((a, b) <- iterator(x)) {
      if (!it.hasNext) return 1
      val (c, d) = it.next
      val s = pp.compare(a, c)
      if (s < 0) return -1
      else if (s > 0) return 1
      else {
        val s = ring.compare(b, d)
        if (s < 0) return -1
        else if (s > 0) return 1
      }
    }
    if (!it.hasNext) 0 else -1
  }
  def isUnit(x: T) = if (x.isZero) false else headCoefficient(x).isUnit
  def times(x: T, y: T) = (zero /: iterator(y)) { (l, r) =>
    val (a, b) = r
    l + multiply(x, a, b)
  }
  override def toCode(x: T, precedence: Int) = {
    var s = ""
    var n = 0
    var m = 0
    for ((a, b) <- iterator(x)) {
      val c = ring.abs(b)
      val (t, u) = {
        if (a.isOne) (c.toCode(0), 1)
        else if (c.isOne) (a.toCode(0), pp.size(a))
        else (c.toCode(1) + "*" + a.toCode(1), 1 + pp.size(a))
      }
      s = {
        if (n == 0) {
          if (ring.signum(b) < 0) "-" + t else t
        } else {
          if (ring.signum(b) < 0) s + "-" + t else s + "+" + t
        }
      }
      m = if (ring.signum(b) < 0) u + 1 else u
      n += 1
    }
    if (n == 0) ring.zero.toCode(0) else {
      val fenced = {
        if (n == 1) {
          if (m == 1) false
          else precedence > 1
        } else precedence > 0
      }
      if (fenced) "(" + s + ")" else s
    }
  }
  override def toString = ring.toString + pp.toString
  def toMathML(x: T) = {
    var s = <sep/>
    var n = 0
    for ((a, b) <- iterator(x)) {
      val c = ring.abs(b)
      val t = {
        if (a.isOne) c.toMathML
        else if (c.isOne) a.toMathML
        else <apply><times/>{c.toMathML}{a.toMathML}</apply>
      }
      s = {
        if (n == 0) {
          if (ring.signum(b) < 0) <apply><minus/>{t}</apply> else t
        } else {
          if (ring.signum(b) < 0) <apply><minus/>{s}{t}</apply> else <apply><plus/>{s}{t}</apply>
        }
      }
      n += 1
    }
    if (n == 0) ring.zero.toMathML else s
  }
  def toMathML = <mrow>{ring.toMathML}{pp.toMathML}</mrow>
  def apply(value: C): T
  def fromPowerProduct(value: Array[N]): T

  def iterator(x: T): Iterator[Pair[Array[N], C]]

  def variables = pp.variables

  def length = variables.length

  def headPowerProduct(x: T) = {
    val (a, b) = headTerm(x)
    a
  }

  def headCoefficient(x: T) = {
    val (a, b) = headTerm(x)
    b
  }

  def headTerm(x: T) = iterator(x).next

  def degree(x: T) = (0l /: iterator(x)) { (l, r) =>
    val (a, b) = r
    scala.math.max(l, pp.degree(a))
  }

  def reduce(x: T, y: T): T = {
    if (x.isZero) zero
    else {
      val (s, a) = headTerm(x)
      val (t, b) = headTerm(y)
      if (!(t | s)) x else {
        reduce(multiply(x, b) - multiply(y, s / t, a), y)
      }
    }
  }

  def multiply(w: T, x: Array[N], y: C) = map(w, (a, b) => (a * x, b * y))

  def multiply(w: T, y: C) = map(w, (a, b) => (a, b * y))

  def map(w: T, f: (Array[N], C) => (Array[N], C)): T
}

object Polynomial {
  trait Element[T <: Element[T, C, N], C, N] extends Ring.Element[T] { this: T =>
    val factory: Polynomial[T, C, N]
  }
}
