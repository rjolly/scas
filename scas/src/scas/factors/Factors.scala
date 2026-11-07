package scas.factors

import scala.compiletime.deferred
import scas.structure.{Monoid, Ring}
import scas.util.{Conversion, unary_~}
import scas.math.Integral
import scas.base.BigInteger
import BigInteger.given
import Factors.Element

trait Factors[T, N : Integral as numeric] extends Monoid[Element[T, N]] {
  given ring: Ring[T] = deferred
  def empty: Element[T, N]
  val zero = fromRing(ring.zero)
  val minusOne = fromRing(-ring.one)
  override val one = fromRing(ring.one)
  def fromRing(x: T) = if x.isOne then empty else empty + ((x, numeric.one))
  def apply(x: T): Element[T, N] = if x.isZero then zero else if x.signum < 0 then this(-x) * minusOne else this(x, false)
  def apply(x: T, dummy: Boolean): Element[T, N]
  extension (x: Element[T, N]) {
    def isZero = x.getOrElse(ring.zero, numeric.zero) >< numeric.one
    override def isOne = x.isEmpty
    def expand = x.foldLeft(ring.one) { (l, r) =>
      val (a, b) = r
      l * a \ b.toLong
    }
    def sqrt = x.map((a, b) => (a, b / numeric.fromInt(2)))
    def isSquare = x.foldLeft(true) { (l, r) =>
      val (_, b) = r
      l && b % numeric.fromInt(2) == 0
    }
    def multiply(y: Element[T, N]) = if x.isZero || y.isZero then zero else y.foldLeft(x)((l, r) => {
      val (a, b) = r
      val c = l.getOrElse(a, numeric.zero)
      if a >< -ring.one && b >< numeric.one && c >< numeric.one then l - a else l + ((a, c + b))
    })
    def isUnit = x.isOne
  }
  def equiv(x: Element[T, N], y: Element[T, N]) = {
    val xs = x.iterator
    val ys = y.iterator
    while xs.hasNext && ys.hasNext do {
      val (a, b) = xs.next()
      val (c, d) = ys.next()
      if a <> c then return false
      else if b <> d then return false
    }
    !xs.hasNext && !ys.hasNext
  }
  extension (x: Element[T, N]) def toCode(level: Level) = {
    var s = ring.one.show
    val p = if x.size == 1 then level else Level.Multiplication
    var m = 0
    for (a, b) <- x do {
      val t = if b >< numeric.one then a.toCode(p) else s"${a.toCode(Level.Power)}\\$b"
      s = if m == 0 then t else s + "*" + t
      m += 1
    }
    s
  }
  override def toString = s"Product($ring)"
  extension (x: Element[T, N]) def toMathML = {
    var s = ring.one.toMathML
    var m = 0
    for (a, b) <- x do {
      val t = if b >< numeric.one then a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
      s = if m == 0 then t else s"<apply><times/>$s$t</apply>"
      m += 1
    }
    s
  }
  def toMathML = s"<apply><cartesianproduct/>${ring.toMathML}</apply>"

  given ring2factors: [U: Conversion[T]] => (U => Element[T, N]) = x => this(~x)
}

object Factors {
  trait Conv[T, N] extends Factors[T, N] with Monoid.Conv[Element[T, N]]

  type Element[T, N] = Map[T, N]
}
