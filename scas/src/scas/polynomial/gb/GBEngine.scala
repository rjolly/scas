package scas.polynomial.gb

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ListBuffer
import scas.polynomial.Polynomial
import scas.power.PowerProduct
import scas.math.Ordering
import scas.prettyprint.Show.given
import java.util.logging.Logger
import GBEngine.{Impl, Pair}

open class GBEngine[T, C, M : PowerProduct](using factory: Polynomial[T, C, M]) extends Impl[T, C, M, Pair[M]] {
  def this(factory: Polynomial[T, C, M]) = this(using factory.pp, factory)

  def apply(i: Int, j: Int, reduction: Boolean, principal: Int, coprime: Boolean, scm: M) = new Pair(i, j, reduction, principal, coprime, scm)

  extension (pair: Pair[M]) def show = "{" + pair.i + ", " + pair.j + "}, " + pair.scm.show + ", " + pair.reduction
}

object GBEngine {
  open class Pair[M](val i: Int, val j: Int, val reduction: Boolean, val principal: Int, val coprime: Boolean, val scm: M)

  trait Impl[T, C, M : PowerProduct as pp, P <: Pair[M]](using factory: Polynomial[T, C, M]) {
    import factory.{normalize, s_polynomial}
    val logger = Logger.getLogger(getClass().getName());

    def process(pa: P): Unit = {
      if !b_criterion(pa) then {
        logger.config(pa.show)
        val p = normalize(s_polynomial(polys(pa.i), polys(pa.j)).reduce(polys.toSeq*))
        if !p.isZero then update(p)
        npairs += 1
      }
      remove(pa)
    }
    extension (x: T) {
      def reduce(ys: T*) = x.reduce(false, false, ys*)
      def reduce(tail: Boolean, ys: T*) = x.reduce(false, tail, ys*)
    }
    def b_criterion(pa: P): Boolean = {
      var k = 0
      while k < polys.size do {
        if (k.headPowerProduct | pa.scm) && considered(pa.i, k) && considered(pa.j, k) then return true
        k += 1
      }
      false
    }
    def remove(pa: P): Unit = {
      pairs -= pa
      if pa.reduction then removed += pa.principal
      logger.fine("remove " + pa.show)
    }

    def apply(i: Int, j: Int): P = apply(i, j, i.headPowerProduct, j.headPowerProduct)
    def apply(i: Int, j: Int, m: M, n: M): P = apply(i, j, if m < n then m | n else n | m, if m < n then j else i, pp.coprime(m, n), pp.lcm(m, n))
    def apply(i: Int, j: Int, reduction: Boolean, principal: Int, coprime: Boolean, scm: M): P

    extension (pa: P) def show: String

    def sorted(i: Int, j: Int) = if i > j then apply(j, i) else apply(i, j)
    def make(index: Int): Unit = for i <- 0 until index do {
      val pa = apply(i, index)
      pairs += pa
      if pa.coprime then remove(pa)
    }
    def considered(i: Int, j: Int) = !pairs.contains(sorted(i, j))

    def ordering: Ordering[P] = Ordering by { pair => (pair.scm, pair.j, pair.i) }
    given Ordering[P] = ordering

    var pairs = SortedSet.empty[P]
    var removed = SortedSet.empty[Int]
    val polys = ListBuffer.empty[T]
    var npairs = 0
    var npolys = 0

    extension (i: Int) {
      def show = i.headPowerProduct.show
      def headPowerProduct = polys(i).headPowerProduct
    }

    def gb(xs: T*) = {
      update(xs)
      process
      reduce
      toList
    }

    def update(s: Seq[T]): Unit = {
      logger.config(s.toList.show)
      s.foreach { p =>
        if !p.isZero then update(p)
      }
      npairs = 0
      npolys = 0
    }

    def update(poly: T): Unit = {
      polys += poly
      val index = polys.size - 1
      logger.config("(" + index.show + ", " + index + ")")
      make(index)
      npolys += 1
    }

    def process: Unit = {
      logger.config("process")
      while !pairs.isEmpty do process(pairs.head)
    }

    def reduce: Unit = {
      logger.config("reduce")
      var i = 0
      var j = 0
      while i < polys.size do {
        if removed.contains(i + j) then {
          removed -= i + j
          polys.remove(i)
          j += 1
        } else i += 1
      }
      for i <- 0 until polys.size do {
        polys(i) = normalize(polys(i).reduce(true, polys.toSeq*))
        logger.config("(" + i.headPowerProduct.show + ")")
      }
    }

    def toList = {
      logger.config("statistics = (" + npairs + ", " + npolys + ", " + polys.size + ")")
      polys.toList
    }
  }
}
