package scas.polynomial

import java.util.TreeSet
import scala.jdk.CollectionConverters.SetHasAsScala
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.math.Ordering

trait PolynomialWithEngine[T : ClassTag, C, M] extends PolynomialOverUFD[T, C, M] {
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

  class Pair(val i: Int, val j: Int, val m: M, val n: M, val scm: M) {
    def key = (scm, j, i)
    def reduction = if (m < n) m | n else n | m
    def principal = if (m < n) j else i
    def coprime = pp.coprime(m, n)
  }

  def process(pa: P): Unit = {
    if(!b_criterion(pa)) {
      println("{" + pa.i + ", " + pa.j + "}, " + pa.scm.show + ", " + pa.reduction)
      val p = normalize(s_polynomial(polys(pa.i), polys(pa.j)).reduce(polys.toSeq))
      if (!p.isZero) update(p)
      npairs += 1
    }
    remove(pa)
  }
  def b_criterion(pa: P): Boolean = {
    var k = 0
    while (k < polys.size) {
      if ((headPowerProduct(k) | pa.scm) && considered(pa.i, k) && considered(pa.j, k)) return true
      k += 1
    }
    false
  }
  def remove(pa: P): Unit = {
    pairs.remove(pa)
    if(pa.reduction) removed(pa.principal) = true
  }
  def add(pa: P): Unit = {
    pairs.add(pa)
    if (pa.coprime) remove(pa)
  }

  def apply(i: Int, j: Int): P
  def sorted(i: Int, j: Int) = if (i > j) apply(j, i) else apply(i, j)
  def make(index: Int): Unit = for (i <- 0 until index) add(apply(i, index))
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
    while(!pairs.isEmpty) process(pairs.asScala.head)
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
