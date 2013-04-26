package scas.gb

import scala.collection.SortedSet
import scala.collection.mutable.ArrayBuffer
import scas.math.Ordering.Tuple3
import scas.polynomial.PolynomialWithGB
import scas.Implicits.{infixOrderingOps, infixRingOps, infixPowerProductOps}
import PolynomialWithGB.Element

class PairList[T <: Element[T, C, N], C, N](val ring: PolynomialWithGB[T, C, N]) {
  import ring.{pp, s_polynomial, normalize, cm}

  type Pair = (Array[N], Int, Int)

  var pairs = SortedSet.empty[Pair]
  val removed = new ArrayBuffer[Boolean]
  val polys = new ArrayBuffer[T]
  var npairs = 0
  var npolys = 0

  def toSeq = {
    println("signature = (" + npairs + ", " + npolys + ", " + polys.size + ")")
    polys.toSeq
  }

  def update(s: Seq[T]): Unit = {
    println(s)
    s.foreach { p =>
      if (!p.isZero) add(p)
    }
    npairs = 0
    npolys = 0
  }

  def process: Unit = {
    println("process")
    while(!pairs.isEmpty) {
      val pa = pairs.head;
      process(pa);
      remove(pa);
    }
  }

  def process(pair: Pair): Unit = {
    if(!b_criterion(pair)) {
      val p = reduce(pair)
      if (!p.isZero) add(p)
      npairs += 1
    }
  }

  def reduce(pair: Pair) = {
    val (scm, i, j) = pair
    println("{" + i + ", " + j + "}, " + pp.degree(scm) + ", " + reduction(pair))
    normalize(ring.reduce(s_polynomial(polys(i), polys(j)), polys))
  }

  def add(poly: T): Unit = {
    polys += poly
    removed += false
    val index = polys.size - 1
    println("(" + headPowerProduct(index).toCode(0) + ", " + index + ")")
    makePairs(index)
    npolys += 1
  }

  def makePairs(index: Int): Unit = for (i <- 0 until index) {
    val pa = pair(i, index)
    pairs += pa
    if (coprime(pa)) remove(pa)
  }

  def remove(pair: Pair): Unit = {
    pairs -= pair
    if(reduction(pair)) removed(principal(pair)) = true
  }

  def b_criterion(pair: Pair): Boolean = {
    val (scm, i, j) = pair
    for (k <- 0 until polys.size) {
      if ((headPowerProduct(k) | scm) && considered(i, k) && considered(j, k)) return true
    }
    return false
  }

  def headPowerProduct(i: Int): Array[N] = ring.headPowerProduct(polys(i))

  def considered(i: Int, j: Int) = !pairs.contains(sorted(i, j))

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

  def sorted(i: Int, j: Int) = if (i > j) pair(j, i) else pair(i, j)

  def pair(i: Int, j: Int) = {
    val m = headPowerProduct(i)
    val n = headPowerProduct(j)
    (pp.scm(m, n), i, j)
  }

  def reduction(pair: Pair) = {
    val (_, i, j) = pair
    val m = headPowerProduct(i)
    val n = headPowerProduct(j)
    if (m < n) m | n else n | m
  }

  def principal(pair: Pair) = {
    val (_, i, j) = pair
    val m = headPowerProduct(i)
    val n = headPowerProduct(j)
    if (m < n) j else i
  }

  def coprime(pair: Pair) = {
    val (_, i, j) = pair
    val m = headPowerProduct(i)
    val n = headPowerProduct(j)
    pp.coprime(m, n)
  }
}
