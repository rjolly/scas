package scas.gb

import scala.collection.SortedSet
import scas.math.Ordering.Tuple3
import scas.polynomial.PolynomialWithGB
import scas.Implicits.{infixOrderingOps, infixRingOps, infixPowerProductOps}
import PolynomialWithGB.Element

class PairList[T <: Element[T, C, N], C, N](list: List[T])(implicit val ring: PolynomialWithGB[T, C, N]) {
  import ring.{pp, s_polynomial, normalize, headPowerProduct}

  type Pair = (Array[N], T, T)

  var pairs = SortedSet.empty[Pair]
  var removed = SortedSet.empty[T]
  var polys = List.empty[T]
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
    polys
  }

  def process(pair: Pair): Unit = {
    if(!b_criterion(pair)) {
      val p = reduce(pair)
      if (!p.isZero) add(p)
      npairs += 1
    }
  }

  def reduce(pair: Pair) = {
    val (scm, x, y) = pair
    println("{" + x.index + ", " + y.index + "}, " + pp.degree(scm) + ", " + reduction(pair))
    normalize(ring.reduce(s_polynomial(x, y), polys))
  }

  def add(poly: T): Unit = {
    val p = ring(poly, polys.size)
    println("(" + headPowerProduct(p).toCode(0) + ", " + p.index + ")")
    makePairs(p)
    polys = p::polys
    npolys += 1
  }

  def makePairs(polynomial: T): Unit = polys.foreach { p =>
    val pa = pair(p, polynomial)
    pairs += pa
    if (coprime(pa)) remove(pa)
  }

  def remove(pair: Pair): Unit = {
    pairs -= pair
    if(reduction(pair)) removed += principal(pair)
  }

  def b_criterion(pair: Pair) = {
    val (scm, x, y) = pair
    (false /: polys) { (l, r) => l || ((headPowerProduct(r) | scm) && considered(x, r) && considered(y, r)) }
  }

  def considered(x: T, y: T) = !pairs.contains(sorted(x, y))

  def remove: Unit = {
    polys = polys.filter(!removed.contains(_))
  }

  def reduce: Unit = {
    println("reduce");
    var s = SortedSet.empty[T]
    val size = polys.size
    for (i <- 0 until size) {
      val p = normalize(ring.reduce(polys(i), polys, true))
      polys = polys.updated(i, p)
      println("(" + headPowerProduct(p).toCode(0) + ")")
      s += p
    }
    polys = s.toList
  }

  def pair(x: T, y: T) = (pp.scm(headPowerProduct(x), headPowerProduct(y)), x, y)

  def sorted(x: T, y: T) = if (x > y) pair(y, x) else pair(x, y)

  def reduction(pair: Pair) = {
    val (scm, x, y) = pair
    val m = headPowerProduct(x)
    val n = headPowerProduct(y)
    if (m < n) m | n else n | m
  }

  def principal(pair: Pair) = {
    val (scm, x, y) = pair
    val m = headPowerProduct(x)
    val n = headPowerProduct(y)
    if (m < n) y else x
  }

  def coprime(pair: Pair) = {
    val (scm, x, y) = pair
    pp.coprime(headPowerProduct(x), headPowerProduct(y))
  }
}
