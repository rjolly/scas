package scas.polynomial

import java.util.TreeSet
import scala.jdk.CollectionConverters.SetHasAsScala
import scala.collection.mutable.ListBuffer
import scala.math.Ordering

class Engine[T, C, M](using factory: PolynomialWithGB[T, C, M]) {
  import factory.{normalize, s_polynomial, pp}

  def process(pa: Pair[M]): Unit = {
    if(!b_criterion(pa)) {
      println(pa)
      val p = normalize(s_polynomial(polys(pa.i), polys(pa.j)).reduce(polys.toSeq))
      if (!p.isZero) update(p)
      npairs += 1
    }
    remove(pa)
  }
  def b_criterion(pa: Pair[M]): Boolean = {
    var k = 0
    while (k < polys.size) {
      if ((headPowerProduct(k) | pa.scm) && considered(pa.i, k) && considered(pa.j, k)) return true
      k += 1
    }
    false
  }
  def remove(pa: Pair[M]): Unit = {
    pairs.remove(pa)
    if(pa.reduction) removed(pa.principal) = true
  }
  def add(pa: Pair[M]): Unit = {
    pairs.add(pa)
    if (pa.coprime) remove(pa)
  }

  def apply(i: Int, j: Int) = {
    val m = headPowerProduct(i)
    val n = headPowerProduct(j)
    val scm = pp.lcm(m, n)
    new Pair(i, j, m, n, scm)
  }
  def sorted(i: Int, j: Int) = if (i > j) apply(j, i) else apply(i, j)
  def make(index: Int): Unit = for (i <- 0 until index) add(apply(i, index))
  def considered(i: Int, j: Int) = !pairs.contains(sorted(i, j))

  given ordering: Ordering[Pair[M]] = Ordering by { (pair: Pair[M]) => pair.key }

  var pairs = new TreeSet(ordering)
  val removed = new ListBuffer[Boolean]
  val polys = new ListBuffer[T]
  var npairs = 0
  var npolys = 0

  def headPowerProduct(i: Int) = factory.headPowerProduct(polys(i))

  def process(xs: Seq[T]): List[T] = {
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
    val a = polys.toList
    polys.clear
    a
  }
}
