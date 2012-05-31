package scas.polynomial

import scas.structure.Ring
import scas.polynomial.ordering.Ordering
import scas.Implicits.{infixRingOps, infixPowerProductOps}
import Polynomial.Element

trait Polynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Ring[T] {
  implicit val ring: Ring[C]
  implicit val pp: PowerProduct[N]
  implicit val cm: ClassManifest[T]
  def generators = pp.generators.map(fromPowerProduct)
  def generatorsBy(n: Int) = pp.generatorsBy(n).map(_.map(fromPowerProduct))
  override def signum(x: T) = if (x.isZero) 0 else { val (a, b) = last(x) ; ring.signum(b) }
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
  def isUnit(x: T) = if (degree(x) > 0 || x.isZero) false else headCoefficient(x).isUnit
  def times(x: T, y: T) = (zero /: iterator(y)) { (l, r) =>
    val (a, b) = r
    l + multiply(x, a, b)
  }
  override def toCode(x: T, precedence: Int) = {
    var s = ""
    var n = 0
    var m = 0
    for ((a, b) <- reverseIterator(x)) {
      val c = ring.abs(b)
      val g = ring.signum(b) < 0
      val (t, u) = {
        if (a.isOne) (c.toCode(0), 1)
        else if (c.isOne) (a.toCode(0), pp.size(a))
        else (c.toCode(1) + "*" + a.toCode(1), 1 + pp.size(a))
      }
      s = {
        if (n == 0) {
          if (g) "-" + t else t
        } else {
          if (g) s + "-" + t else s + "+" + t
        }
      }
      m = if (g) u + 1 else u
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
    for ((a, b) <- reverseIterator(x)) {
      val c = ring.abs(b)
      val g = ring.signum(b) < 0
      val t = {
        if (a.isOne) c.toMathML
        else if (c.isOne) a.toMathML
        else <apply><times/>{c.toMathML}{a.toMathML}</apply>
      }
      s = {
        if (n == 0) {
          if (g) <apply><minus/>{t}</apply> else t
        } else {
          if (g) <apply><minus/>{s}{t}</apply> else <apply><plus/>{s}{t}</apply>
        }
      }
      n += 1
    }
    if (n == 0) ring.zero.toMathML else s
  }
  def toMathML = <mrow>{ring.toMathML}{pp.toMathML}</mrow>
  def apply(value: C): T
  def fromPowerProduct(value: Array[N]): T

  def iterator(x: T): Iterator[(Array[N], C)]

  def iterator(x: T, m: Array[N]): Iterator[(Array[N], C)]

  def reverseIterator(x: T): Iterator[(Array[N], C)]

  def variables = pp.variables

  def length = variables.length

  def head(x: T): (Array[N], C)

  def headPowerProduct(x: T) = { val (a, b) = head(x) ; a }

  def headCoefficient(x: T) = { val (a, b) = head(x) ; b }

  def last(x: T): (Array[N], C)

  def lastCoefficient(x: T) = { val (a, b) = last(x) ; b }

  def degree(x: T) = (0l /: iterator(x)) { (l, r) =>
    val (a, b) = r
    scala.math.max(l, pp.degree(a))
  }

  def reduce(x: T, y: T): T = reduce(x, List(y))

  def reduce(x: T, list: List[T]): T = {
    val it = iterator(x)
    if (it.hasNext) {
      val (s, a) = it.next
      list match {
        case y::tail => {
          val (t, b) = head(y)
          if (!(t | s)) reduce(x, tail) else {
            reduce(subtract(x, s / t, a, y, b), list)
          }
        }
        case _ => x
      }
    } else x
  }

  def reduce(x: T, list: List[T], tail: Boolean): T = {
    if (tail && !x.isZero) reduce(x, headPowerProduct(x), list) else reduce(x, list)
  }

  def reduce(x: T, m: Array[N], list: List[T]): T = {
    val it = iterator(x, m)
    if (it.hasNext) {
      val (s, a) = it.next
      list match {
        case y::tail => {
          val (t, b) = head(y)
          if (!(t | s)) reduce(x, m, tail) else {
            reduce(subtract(x, s / t, a, y, b), m, list)
          }
        }
        case _ => {
          if (it.hasNext) {
            val (s, a) = it.next
            reduce(x, s, list)
          } else x
        }
      }
    } else x
  }

  def subtract(x: T, m: Array[N], a: C, y: T, b: C) = multiply(x, b) - multiply(y, m, a)

  def multiply(x: T, m: Array[N], c: C) = map(x, (s, a) => (s * m, a * c))

  def multiply(x: T, c: C) = map(x, (s, a) => (s, a * c))

  def map(x: T, f: (Array[N], C) => (Array[N], C)): T
}

object Polynomial {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Ring.Element[T] { this: T =>
    val factory: Polynomial[T, C, N]
  }
}
