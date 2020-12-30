package scas.polynomial

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scas.structure.Ring
import scas.power.PowerProduct

abstract class Polynomial[T : ClassTag, C : Ring, M : PowerProduct] extends Ring[T] {
  def ring = Ring[C]
  def pp = PowerProduct[M]
  val zero = this()
  def generator(n: Int) = fromPowerProduct(pp.generator(n))
  def generators = pp.generators.map(fromPowerProduct)
  extension (x: T) def signum = if (x.isZero) 0 else lastCoefficient(x).signum
  def characteristic = ring.characteristic
  override def apply(x: T) = sort(x.map((s, a) => (pp(s), ring(a))))
  val one = fromRing(ring.one)
  extension (x: T) def - (y: T) = x.subtract(pp.one, ring.one, y)
  def equiv(x: T, y: T) = {
    val xs = iterator(x)
    val ys = iterator(y)
    while (xs.hasNext && ys.hasNext) {
      if (!equiv(xs.next, ys.next)) return false
    }
    !xs.hasNext && !ys.hasNext
  }
  def equiv(x: (M, C), y: (M, C)) = {
    val (s, a) = x
    val (t, b) = y
    s >< t && a >< b
  }
  extension (x: T) def isUnit = if (degree(x) > 0 || x.isZero) false else headCoefficient(x).isUnit
  extension (x: T) {
    def * (y: T) = {
      var r = zero
      for ((a, b) <- iterator(x)) r = r.subtract(a, -b, y)
      r
    }
    def %* (m: M) = x.map((s, a) => (s * m, a))
  }

  extension (x: T) def toCode(level: Level) = {
    var s = ring.zero.toCode(Level.Addition)
    var n = 0
    var m = 0
    val p = if (size(x) == 1) level else Level.Addition
    for ((a, b) <- reverseIterator(x)) {
      val c = ring.abs(b)
      val g = b.signum < 0
      val (t, u) = if (a.isOne) (c.toCode(p), 1) else if (c.isOne) (a.toCode(p), pp.size(a)) else (c.toCode(Level.Multiplication) + "*" + a.toCode(Level.Multiplication), 1 + pp.size(a))
      s = if (n == 0) {
        if (g) "-" + t else t
      } else {
        if (g) s + "-" + t else s + "+" + t
      }
      m = if (g) u + 1 else u
      n += 1
    }
    if (n == 0) s else if (n == 1) {
      if (m == 1) s else if (level > Level.Multiplication) fenced(s) else s
    } else {
      if (level > Level.Addition) fenced(s) else s
    }
  }
  override def toString = s"$ring(${pp.variables.mkString(", ")})"
  extension (x: T) def toMathML = {
    var s = ring.zero.toMathML
    var n = 0
    for ((a, b) <- reverseIterator(x)) {
      val c = ring.abs(b)
      val g = b.signum < 0
      val t =  if (a.isOne) c.toMathML else if (c.isOne) a.toMathML else s"<apply><times/>${c.toMathML}${a.toMathML}</apply>"
      s = if (n == 0) {
        if (g) s"<apply><minus/>$t</apply>" else t
      } else {
        if (g) s"<apply><minus/>$s$t</apply>" else s"<apply><plus/>$s$t</apply>"
      }
      n += 1
    }
    s
  }
  def toMathML = s"<apply>${ring.toMathML}${pp.toMathML}</apply>"

  extension (ring: Ring[C]) def apply(s: T*) = this

  def fromRing(value: C) = if(value.isZero) zero else this(pp.one, value)
  def fromPowerProduct(value: M) = this(value, ring.one)
  def apply(m: M, c: C): T = this((m, c))
  def apply(s: (M, C)*): T

  def iterator(x: T): Iterator[(M, C)]

  extension (x: T) def iterator(m: M): Iterator[(M, C)] = this.iterator(x).dropWhile((s, _) => s > m)

  def reverseIterator(x: T) = x.toSeq.reverseIterator

  extension (x: T) def toSeq = this.iterator(x).toSeq

  def variables = pp.variables

  def size(x: T): Int

  def head(x: T): (M, C)

  def headPowerProduct(x: T) = {
    val (a, _) = head(x)
    a
  }

  def headCoefficient(x: T) = {
    val (_, b) = head(x)
    b
  }

  def last(x: T): (M, C)

  def lastCoefficient(x: T) = {
    val (_, b) = last(x)
    b
  }

  extension (x: T) def coefficient(m: M) = {
    val xs = x.iterator(m)
    if (xs.hasNext) {
      val (s, a) = xs.next
      if (s >< m) a else ring.zero
    } else ring.zero
  }

  def degree(x: T) = {
    var d = 0l
    for ((a, _) <- iterator(x)) d = scala.math.max(d, pp.degree(a))
    d
  }

  extension (x: T) @tailrec final def reduce(ys: Iterator[T]): T = {
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

  extension (y: T) def reduce(s: M) = {
    val (t, _) = head(y)
    t | s
  }

  extension (x: T) def reduce(ys: Iterator[T], tail: Boolean): T = {
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

  extension (x: T) @tailrec final def reduce(m: M, ys: Iterator[T]): T = {
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

  extension (x: T) {
    def reduce(m: M, a: C, y: T, b: C) = x.multiply(b).subtract(m, a, y)

    def subtract(m: M, c: C, y: T) = x + y.multiply(m, -c)

    def multiply(m: M, c: C) = x.map((s, a) => (s * m, a * c))

    def multiply(c: C) = x.map(a => a * c)

    def map(f: C => C): T = x.map((s, a) => (s, f(a)))

    def map(f: (M, C) => (M, C)): T
  }

  def sort(x: T) = this(x.toSeq.sortBy((s, _) => s)(pp.reverse): _*)
}
