package scas.polynomial

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scas.structure.Ring
import scas.power.PowerProduct

abstract class Polynomial[T : ClassTag, C : Ring, N : PowerProduct] extends Ring[T] {
  def ring = Ring[C]
  def pp = PowerProduct[N]
  def zero = apply()
  def generator(n: Int) = fromPowerProduct(pp.generator(n))
  def generators = pp.generators.map(fromPowerProduct)
  def signum(x: T) = if (x >< zero) 0 else ring.signum(lastCoefficient(x))
  def characteristic = ring.characteristic
  override def apply(x: T) = sort(x.map((s, a) => (pp(s), ring(a))))
  def one = fromRing(ring.one)
  def (x: T) - (y: T) = x.subtract(pp.one, ring.one, y)
  def equiv(x: T, y: T) = {
    val xs = iterator(x)
    val ys = iterator(y)
    while (xs.hasNext && ys.hasNext) {
      if (!equiv(xs.next, ys.next)) return false
    }
    !xs.hasNext && !ys.hasNext
  }
  def equiv(x: (Array[N], C), y: (Array[N], C)) = {
    val (s, a) = x
    val (t, b) = y
    s >< t && a >< b
  }
  def (x: T).isUnit = if (degree(x) > 0 || x >< zero) false else headCoefficient(x).isUnit
  def (x: T) * (y: T) = iterator(y).foldLeft(zero) { (l, r) =>
    val (a, b) = r
    l.subtract(a, -b, x)
  }
  def (x: T)%* (m: Array[N]) = x.map((s, a) => (s * m, a))

  def (x: T).toCode(level: Level) = {
    var s = ring.zero.toCode(Level.Addition)
    var n = 0
    var m = 0
    val p = if (size(x) == 1) level else Level.Addition
    for ((a, b) <- reverseIterator(x)) {
      val c = ring.abs(b)
      val g = ring.signum(b) < 0
      val (t, u) = {
        if (a >< pp.one) (c.toCode(p), 1)
        else if (c >< ring.one) (a.toCode(p), pp.size(a))
        else (s"${c.toCode(Level.Multiplication)}*${a.toCode(Level.Multiplication)}", 1 + pp.size(a))
      }
      s = {
        if (n == 0) {
          if (g) s"-${t}" else t
        } else {
          if (g) s"${s}-${t}" else s"${s}+${t}"
        }
      }
      m = if (g) u + 1 else u
      n += 1
    }
    val fenced = {
      if (n == 0) false
      else if (n == 1) {
        if (m == 1) false
        else level > Level.Multiplication
      } else level > Level.Addition
    }
    if (fenced) s"(${s})" else s
  }

  def (x: T).toMathML = {
    var s = ring.zero.toMathML
    var n = 0
    for ((a, b) <- reverseIterator(x)) {
      val c = ring.abs(b)
      val g = ring.signum(b) < 0
      val t = {
        if (a >< pp.one) c.toMathML
        else if (c >< ring.one) a.toMathML
        else s"<apply><times/>${c.toMathML}${a.toMathML}</apply>"
      }
      s = {
        if (n == 0) {
          if (g) s"<apply><minus/>${t}</apply>" else t
        } else {
          if (g) s"<apply><minus/>${s}${t}</apply>" else s"<apply><plus/>${s}${t}</apply>"
        }
      }
      n += 1
    }
    s
  }

  def fromRing(value: C) = if(value >< Ring[C].zero) zero else apply(PowerProduct[N].one, value)
  def fromPowerProduct(value: Array[N]) = apply(value, Ring[C].one)
  def apply(m: Array[N], c: C): T = apply((m, c))
  def apply(s: (Array[N], C)*): T

  def iterator(x: T): Iterator[(Array[N], C)]

  def (x: T).iterator(m: Array[N]): Iterator[(Array[N], C)] = iterator(x).dropWhile { r =>
    val (s, _) = r
    s > m
  }

  def reverseIterator(x: T) = toSeq(x).reverseIterator

  def toSeq(x: T) = iterator(x).toSeq

  def variables = pp.variables

  def size(x: T): Int

  def head(x: T): (Array[N], C)

  def headPowerProduct(x: T) = { val (a, _) = head(x) ; a }

  def headCoefficient(x: T) = { val (_, b) = head(x) ; b }

  def last(x: T): (Array[N], C)

  def lastCoefficient(x: T) = { val (_, b) = last(x) ; b }

  def (x: T).coefficient(m: Array[N]) = {
    val xs = x.iterator(m)
    if (xs.hasNext) {
      val (s, a) = xs.next
      if (s >< m) a else ring.zero
    } else ring.zero
  }

  def degree(x: T) = iterator(x).foldLeft(0l) { (l, r) =>
    val (a, _) = r
    scala.math.max(l, pp.degree(a))
  }

  @tailrec final def (x: T).reduce(ys: Iterator[T]): T = {
    val xs = iterator(x)
    if (xs.hasNext) {
      val (s, a) = xs.next
      ys.find(_.reduce(s)) match {
        case Some(y) => {
          val (t, b) = head(y)
          x.reduce(s / t, a, y, b).reduce(ys)
        }
        case None => x
      }
    } else x
  }

  def (y: T).reduce(s: Array[N]) = {
    val (t, _) = head(y)
    t | s
  }

  def (x: T).reduce(ys: Iterator[T], tail: Boolean): T = {
    val xs = iterator(x)
    if (xs.hasNext) {
      val (s, a) = xs.next
      if (tail) {
        if (xs.hasNext) {
          val (s, a) = xs.next
          x.reduce(s, ys)
        } else x
      } else x.reduce(s, ys)
    } else x
  }

  @tailrec final def (x: T).reduce(m: Array[N], ys: Iterator[T]): T = {
    val xs = x.iterator(m)
    if (xs.hasNext) {
      val (s, a) = xs.next
      ys.find(_.reduce(s)) match {
        case Some(y) => {
          val (t, b) = head(y)
          x.reduce(s / t, a, y, b).reduce(m, ys)
        }
        case None => {
          if (xs.hasNext) {
            val (s, a) = xs.next
            x.reduce(s, ys)
          } else x
        }
      }
    } else x
  }

  def (x: T).reduce(m: Array[N], a: C, y: T, b: C) = x.multiply(b).subtract(m, a, y)

  def (x: T).subtract(m: Array[N], c: C, y: T) = x + y.multiply(m, -c)

  def (x: T).multiply(m: Array[N], c: C) = x.map((s, a) => (s * m, a * c))

  def (x: T).multiply(c: C) = x.map(a => a * c)

  def (x: T).map(f: C => C): T = x.map((s, a) => (s, f(a)))

  def (x: T).map(f: (Array[N], C) => (Array[N], C)): T

  def sort(x: T) = apply(toSeq(x).sortBy({ r =>
    val (s, _) = r
    s
  })(pp.reverse): _*)
}
