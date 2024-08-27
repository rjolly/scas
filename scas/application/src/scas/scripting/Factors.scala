package scas.scripting

import scas.structure.Ring
import scas.math.Numeric
import scas.base.BigInteger
import BigInteger.given
import Factors.Element

abstract class Factors[T, N](using ring: Ring[T], numeric: Numeric[N]) extends Ring[Element[T, N]] {
  def empty: Element[T, N]
  override val zero = empty + ((ring.zero, numeric.one))
  override val one = empty
  def fromInt(n: BigInteger) = apply(ring.fromInt(n))
  def apply(x: T): Element[T, N] = if (x.isOne) empty else if (x.signum < 0) apply(-x) + ((-ring.one, numeric.one)) else empty + ((x, numeric.one))
  extension (x: Element[T, N]) {
    override def convert = x.mapConvert
    def mapConvert = x.foldLeft(empty) { (l, r) =>
      val (a, b) = r
      l + ((a.convert, b))
    }
    override def isZero = x.getOrElse(ring.zero, numeric.zero) >< numeric.one
    override def isOne = x.isEmpty
    def add(y: Element[T, N]) = {
      val (a, b) = x.partitionContains(y)
      val (_, d) = y.partitionContains(a)
      a * this(b.expand + d.expand)
    }
    def partitionContains(y: Element[T, N]) = x.foldLeft((empty, empty)) { (l, r) =>
      val (a, b) = l
      val (c, d) = r
      if (y.contains(c)) (a + r, b) else (a, b + r)
    }
    def expand = x.foldLeft(ring.one) { (l, r) =>
      val (a, b) = r
      l * a \ b.toLong
    }
    def subtract(y: Element[T, N]) = x + (-y)
    def multiply(y: Element[T, N]) = if (x.isZero || y.isZero) zero else y.foldLeft(x)((l, r) => {
      val (a, b) = r
      val c = l.getOrElse(a, numeric.zero)
      if (a >< -ring.one && b >< numeric.one && c >< numeric.one) l - a else l + ((a, c + b))
    })
    def isUnit = abs(x).isOne
    override def unary_- = x * this(-ring.one)
    def signum = if (x.isZero) 0 else if(x.getOrElse(-ring.one, numeric.zero) >< numeric.one) -1 else 1
  }
  def characteristic = ring.characteristic
  def equiv(x: Element[T, N], y: Element[T, N]) = {
    val xs = x.iterator
    val ys = y.iterator
    while (xs.hasNext && ys.hasNext) {
      val (a, b) = xs.next
      val (c, d) = ys.next
      if (a <> c) return false
      else if (b <> d) return false
    }
    !xs.hasNext && !ys.hasNext
  }
  extension (x: Element[T, N]) def toCode(level: Level) = {
    var s = ring.one.show
    val p = if (x.size == 1) level else Level.Multiplication
    var m = 0
    for ((a, b) <- x) {
      val t = if (b >< numeric.one) a.toCode(p) else s"${a.toCode(Level.Power)}\\$b"
      s = if (m == 0) t else s + "*" + t
      m += 1
    }
    s
  }
  override def toString = s"Product($ring)"
  extension (x: Element[T, N]) def toMathML = {
    var s = ring.one.toMathML
    var m = 0
    for ((a, b) <- x) {
      val t = if (b >< numeric.one) a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
      s = if (m == 0) t else s"<apply><times/>$s$t</apply>"
      m += 1
    }
    s
  }
  def toMathML = s"<apply><cartesianproduct/>${ring.toMathML}</apply>"
}

object Factors {
  type Element[T, N] = Map[T, N]
}
