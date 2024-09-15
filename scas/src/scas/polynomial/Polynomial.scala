package scas.polynomial

import scala.annotation.{tailrec, targetName}
import scala.reflect.ClassTag
import scas.structure.{Ring, AlgebraOverRing}
import scas.module.ArrayModule
import scas.power.PowerProduct
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given
import scas.prettyprint.Show.given

trait Polynomial[T : ClassTag, C, M] extends Ring[T] with AlgebraOverRing[T, C] {
  given ring: Ring[C]
  given pp: PowerProduct[M]
  override val zero = this()
  override val one = this(ring.one)
  def fromInt(n: BigInteger) = this(ring.fromInt(n))
  def generator(n: Int) = this(pp.generator(n))
  def generators = pp.generators.map(apply)
  extension (x: T) def signum = if (x.isZero) 0 else x.lastCoefficient.signum
  def characteristic = ring.characteristic
  extension (x: T) override def convert: T = x.convert(pp)
  extension (x: T) def convert(from: PowerProduct[M]) = x.map((s, a) => (s.convert(from), a)).sort
  extension (x: T) def subtract(y: T) = x.subtract(pp.one, ring.one, y)
  def equiv(x: T, y: T) = {
    val xs = x.iterator
    val ys = y.iterator
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
  extension (x: T) def isUnit = if (x.degree > 0 || x.isZero) false else x.headCoefficient.isUnit
  extension (x: T) {
    def multiply(y: T) = {
      var r = zero
      for ((a, b) <- y.iterator) r = r.subtract(a, -b, x)
      r
    }
    def ppMultiplyRight(m: M) = x.map((s, a) => (s * m, a))
  }

  extension (x: T) def toCode(level: Level) = toCode(level, "+", "*")
  extension (x: T) def toCode(level: Level, plus: String, times: String): String = {
    import Level.given
    var s = ring.zero.show
    var n = 0
    var m = 0
    val p = if (x.size == 1) level else Level.Addition
    for ((a, b) <- reverseIterator(x)) {
      val c = ring.abs(b)
      val g = b.signum < 0
      val (t, u) = if (a.isOne) (c.toCode(p), 1) else if (c.isOne) (a.toCode(p, times), a.size) else (c.toCode(Level.Multiplication) + "*" + a.show, 1 + a.size)
      s = if (n == 0) {
        if (g) "-" + t else t
      } else {
        if (g) s + "-" + t else s + plus + t
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
  override def toString = s"${ring}(${variables.toList.show(false)})"
  extension (x: T) def toMathML = toMathML("plus", "times")
  extension (x: T) def toMathML(plus: String, times: String): String = {
    var s = ring.zero.toMathML
    var n = 0
    for ((a, b) <- reverseIterator(x)) {
      val c = ring.abs(b)
      val g = b.signum < 0
      val t =  if (a.isOne) c.toMathML else if (c.isOne) a.toMathML(times) else s"<apply><times/>${c.toMathML}${a.toMathML}</apply>"
      s = if (n == 0) {
        if (g) s"<apply><minus/>$t</apply>" else t
      } else {
        if (g) s"<apply><minus/>$s$t</apply>" else s"<apply><$plus/>$s$t</apply>"
      }
      n += 1
    }
    s
  }
  def toMathML = toMathML(false)
  def toMathML(fenced: Boolean) = s"<mrow>${ring.toMathML}${if (fenced) "<mfenced>" else "<mfenced open=\"[\" close=\"]\">"}${variables.toList.toMathML(false)}</mfenced></mrow>"

  extension (ring: Ring[C]) def apply(s: T*) = {
    same(s*)
    this
  }
  def same(s: T*): Unit = {
    given ArrayModule[T] = ArrayModule(this)(length)
    assert (s.toArray >< generators.toArray)
  }

  def apply(value: C): T = if(value.isZero) zero else this(pp.one, value)
  @targetName("fromPowerProduct") def apply(value: M): T = this(value, ring.one)
  def apply(m: M, c: C): T = this((m, c))
  def apply(s: (M, C)*): T

  def variables = pp.variables

  def length = pp.length

  extension (x: T) {
    def iterator: Iterator[(M, C)]

    def reverseIterator = x.toSeq.reverseIterator

    def iterator(m: M): Iterator[(M, C)] = x.iterator.dropWhile((s, _) => s > m)

    def toSeq = x.iterator.toSeq

    def size: Int

    def head: (M, C)

    def headPowerProduct = {
      val (a, _) = x.head
      a
    }

    def headCoefficient = {
      val (_, b) = x.head
      b
    }

    def last: (M, C)

    def lastCoefficient = {
      val (_, b) = x.last
      b
    }

    @targetName("coef") def coefficient(y: T): T = this(x.coefficient(y.headPowerProduct))

    def coefficient(m: M) = {
      val xs = x.iterator(m)
      if (xs.hasNext) {
        val (s, a) = xs.next
        if (s >< m) a else ring.zero
      } else ring.zero
    }

    def coefOne = x.coefficient(one)

    def degree: BigInteger = x.iterator.foldLeft(BigInteger.zero) { case (l, (a, _)) =>
      BigInteger.max(l, a.degree)
    }

    def reduce(ys: T*): T = x.reduce(false, ys*)

    @tailrec final def reduce(remainder: Boolean, ys: T*): T = {
      val xs = x.iterator
      if (xs.hasNext) {
        val (s, a) = xs.next
        ys.find(_.reduce(s, a, remainder)) match {
          case Some(y) => {
            val (t, b) = y.head
            x.reduce(s / t, a, y, b, remainder).reduce(remainder, ys*)
          }
          case None => x
        }
      } else x
    }

    def reduce(s: M, a: C, remainder: Boolean) = {
      val (t, _) = x.head
      (t | s)
    }

    def reduce(remainder: Boolean, tail: Boolean, ys: T*): T = {
      if (tail) {
        val xs = x.iterator
        if (xs.hasNext) {
          val (s, a) = xs.next
          if (xs.hasNext) {
            val (s, a) = xs.next
            x.reduce(remainder, s, ys*)
          } else x
        } else x
      } else x.reduce(remainder, ys*).reduce(remainder, true, ys*)
    }

    @tailrec final def reduce(remainder: Boolean, m: M, ys: T*): T = {
      val xs = x.iterator(m)
      if (xs.hasNext) {
        val (s, a) = xs.next
        ys.find(_.reduce(s, a, remainder)) match {
          case Some(y) => {
            val (t, b) = y.head
            x.reduce(s / t, a, y, b, remainder).reduce(remainder, m, ys*)
          }
          case None => {
            if (xs.hasNext) {
              val (s, a) = xs.next
              x.reduce(remainder, s, ys*)
            } else x
          }
        }
      } else x
    }

    def reduce(m: M, a: C, y: T, b: C, remainder: Boolean) = (x%* b).subtract(m, a, y)

    def subtract(m: M, c: C, y: T) = x + y.multiply(m, -c)

    def multiply(m: M, c: C) = x.map((s, a) => (s * m, a * c))

    def multiplyRight(c: C) = x.map((s, a) => (s, a * c))

    def map(f: (M, C) => (M, C)): T

    def sort = this(x.toSeq.sortBy((s, _) => s)(pp.reverse)*)
  }

  extension (c: C) def multiplyLeft(x: T) = x%* c

  extension (ring: Ring[C]) def pow(n: Int) = {
    assert(n == 0)
    this
  }

  given coef2poly[D: Conversion[C]]: (D => T) = x => this(~x)
}
