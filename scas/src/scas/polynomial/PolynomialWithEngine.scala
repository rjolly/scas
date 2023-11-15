package scas.polynomial

import java.util.TreeSet
import scala.jdk.CollectionConverters.SetHasAsScala
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.math.Ordering
import scas.structure.commutative.UniqueFactorizationDomain
import scas.power.PowerProduct

trait PolynomialWithEngine[T : ClassTag, C, M](using ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) extends PolynomialOverUFD[T, C, M] {
  def normalize(x: T) = primitivePart(x)
  def s_polynomial(x: T, y: T) = {
    val (m, a) = head(x)
    val (n, b) = head(y)
    val gcd = pp.gcd(m, n)
    val (m0, n0) = (m / gcd, n / gcd)
    x.ppMultiplyRight(n0).reduce(m0, a, y, b)
  }
  def gcd(x: T, y: T) = {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    val list = gb(p, q)
    (if (list.size == 1) list(0) else one)%* ring.gcd(a, b)
  }

  type P <: Pair

  class Pair(val i: Int, val j: Int) { this: P =>
    val m = headPowerProduct(i)
    val n = headPowerProduct(j)
    val scm = pp.lcm(m, n)
    def key = (scm, j, i)
    def process: Unit = {
      if(!b_criterion) {
        println("{" + i + ", " + j + "}, " + scm.show + ", " + reduction)
        val p = normalize(s_polynomial(polys(i), polys(j)).reduce(polys.toSeq))
        if (!p.isZero) update(p)
        npairs += 1
      }
      remove
    }
    def reduction = if (m < n) m | n else n | m
    def principal = if (m < n) j else i
    def coprime = pp.coprime(m, n)
    def b_criterion: Boolean = {
      var r = false
      for (k <- 0 until polys.size) if (!r) {
        if ((headPowerProduct(k) | scm) && considered(i, k) && considered(j, k)) r = true
      }
      return r
    }
    def remove: Unit = {
      pairs.remove(this)
      if(reduction) removed(principal) = true
    }
    def add: Unit = {
      pairs.add(this)
      if (coprime) remove
    }
  }

  def apply(i: Int, j: Int): P
  def sorted(i: Int, j: Int) = if (i > j) apply(j, i) else apply(i, j)
  def make(index: Int): Unit = for (i <- 0 until index) apply(i, index).add
  def considered(i: Int, j: Int) = !pairs.contains(sorted(i, j))

  given ordering: Ordering[P] = Ordering by { (pair: P) => pair.key }

  var pairs = new TreeSet(ordering)
  val removed = new ArrayBuffer[Boolean]
  val polys = new ArrayBuffer[T]
  var npairs = 0
  var npolys = 0

  def headPowerProduct(i: Int): M = headPowerProduct(polys(i))

  def gb(xs: T*) = {
    update(xs)
    process
    reduce
    toList
  }

  def update(s: Seq[T]): Unit = {
    println(s.toList.show)
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
    println("(" + headPowerProduct(index).show + ", " + index + ")")
    make(index)
    npolys += 1
  }

  def process: Unit = {
    println("process")
    while(!pairs.isEmpty) pairs.asScala.head.process
  }

  def reduce: Unit = {
    println("reduce")
    for (i <- polys.size - 1 to 0 by -1 if removed(i)) {
      removed.remove(i)
      polys.remove(i)
    }
    for (i <- 0 until polys.size) {
      polys(i) = normalize(polys(i).reduce(polys.toSeq, true))
      println("(" + headPowerProduct(i).show + ")")
    }
  }

  def toList = {
    println("signature = (" + npairs + ", " + npolys + ", " + polys.size + ")")
    val a = polys.toArray
    polys.clear
    a.toList
  }
}
