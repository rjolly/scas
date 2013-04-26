package scas.gb

import scala.collection.SortedSet
import scala.collection.mutable.ListBuffer
import scas.math.Ordering.Tuple3
import scas.polynomial.PolynomialWithGB
import scas.Implicits.{infixOrderingOps, infixRingOps, infixPowerProductOps}
import PolynomialWithGB.Element

class PairList[T <: Element[T, C, N], C, N](list: List[T])(implicit val ring: PolynomialWithGB[T, C, N]) {
  import ring.{pp, s_polynomial, normalize}

  type Pair = (Array[N], Int, Int)

  var pairs = SortedSet.empty[Pair]
  var removed = SortedSet.empty[Int]
  val polys = new ListBuffer[T]
  var npairs = 0
  var npolys = 0

  println(list)
  list.foreach { p => if (!p.isZero) add(p) }
  npolys = 0

  def process: List[T] = {
    println("process");
    while(!pairs.isEmpty) {
      val pa = pairs.head;
      process(pa);
      remove(pa);
    }
    remove
    reduce
    println("signature = (" + npairs + ", " + npolys + ", " + polys.size + ")")
    polys.toList
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
    normalize(ring.reduce(s_polynomial(polys(i), polys(j)), polys.toList))
  }

  def add(poly: T): Unit = {
    polys += poly
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
    if(reduction(pair)) removed += principal(pair)
  }

  def b_criterion(pair: Pair) = {
    val (scm, i, j) = pair
    (false /: (0 until polys.size)) { (l, r) =>
      val m = headPowerProduct(r)
      l || ((m | scm) && considered(i, r) && considered(j, r))
    }
  }

  def considered(i: Int, j: Int) = !pairs.contains(sorted(i, j))

  def sorted(i: Int, j: Int) = if (i > j) pair(j, i) else pair(i, j)

  def remove: Unit = {
    val s = new ListBuffer[T]
    for (i <- 0 until polys.size if !removed.contains(i)) s += polys(i)
    polys.clear
    polys ++= s
  }

  def reduce: Unit = {
    println("reduce");
    val s = new ListBuffer[T]
    for (i <- 0 until polys.size) {
      polys(i) = normalize(ring.reduce(polys(i), polys.toList, true))
      println("(" + headPowerProduct(i).toCode(0) + ")")
      s += polys(i)
    }
    polys.clear
    polys ++= s
  }

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

  def headPowerProduct(i: Int): Array[N] = ring.headPowerProduct(polys(i))
}
