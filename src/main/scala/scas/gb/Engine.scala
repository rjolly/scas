package scas.gb

import scala.collection.SortedSet
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering
import scas.polynomial.{PolynomialOverUFD, PolynomialWithGB}
import scas.Implicits.{infixOrderingOps, infixRingOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait Engine[T <: Element[T, C, N], C, N] {
  val ring: PolynomialWithGB[T, C, N]
  import ring.{pp, s_polynomial, normalize}

  type P <: Pair

  class Pair(val i: Int, val j: Int) { this: P =>
    def scm = pp.scm(m, n)
    def m = headPowerProduct(i)
    def n = headPowerProduct(j)
    def key = (scm, i, j)
    def process: Unit = {
      if(!b_criterion) {
        val p = reduce
        if (!p.isZero) update(p)
        npairs += 1
      }
      remove
    }
    def reduce = {
      println("{" + i + ", " + j + "}, " + pp.degree(scm) + ", " + reduction)
      normalize(ring.reduce(s_polynomial(polys(i), polys(j)), polys))
    }
    def reduction = if (m < n) m | n else n | m
    def principal = if (m < n) j else i
    def coprime = pp.coprime(m, n)
    def b_criterion: Boolean = {
      for (k <- 0 until polys.size) {
        if ((headPowerProduct(k) | scm) && considered(i, k) && considered(j, k)) return true
      }
      return false
    }
    def remove: Unit = {
      pairs -= this
      if(reduction) removed(principal) = true
    }
    def add: Unit = {
      pairs += this
      if (coprime) remove
    }
  }

  def apply(i: Int, j: Int): P
  def sorted(i: Int, j: Int) = if (i > j) apply(j, i) else apply(i, j)
  def make(index: Int): Unit = for (i <- 0 until index) apply(i, index).add
  def considered(i: Int, j: Int) = !pairs.contains(sorted(i, j))

  implicit def ordering = Ordering by { pair: P => pair.key }

  var pairs = SortedSet.empty[P]
  val removed = new ArrayBuffer[Boolean]
  val polys = new ArrayBuffer[T]
  var npairs = 0
  var npolys = 0

  def headPowerProduct(i: Int): Array[N] = ring.headPowerProduct(polys(i))

  def update(s: Seq[T]): Unit = {
    println(s)
    s.foreach { p =>
      if (!p.isZero) update(p)
    }
    npairs = 0
    npolys = 0
  }

  def update(poly: T): Unit = {
    polys += poly
    removed += false
    val index = polys.size - 1
    println("(" + headPowerProduct(index).toCode(0) + ", " + index + ")")
    make(index)
    npolys += 1
  }

  def process: Unit = {
    println("process")
    while(!pairs.isEmpty) pairs.head.process
  }

  def reduce: Unit = {
    println("reduce")
    for (i <- polys.size - 1 to 0 by -1 if removed(i)) {
      removed.remove(i)
      polys.remove(i)
    }
    for (i <- 0 until polys.size) {
      polys(i) = normalize(ring.reduce(polys(i), polys, true))
      println("(" + headPowerProduct(i).toCode(0) + ")")
    }
  }

  def toSeq = {
    println("signature = (" + npairs + ", " + npolys + ", " + polys.size + ")")
    polys.toSeq
  }
}

object Engine {
  def apply[T <: Element[T, C, N], C, N](ring: PolynomialWithGB[T, C, N]): Engine[T, C, N] = new GB(ring)
}
