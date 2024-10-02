package scas.polynomial

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ListBuffer
import scala.math.Ordering
import scas.prettyprint.Show.given
import java.util.logging.Logger

class Engine[T, C, N](using factory: PolynomialWithGB[T, C, N]) {
  import factory.{normalize, s_polynomial, pp}
  val logger = Logger.getLogger(getClass().getName());

  def process(pa: Pair[N]): Unit = {
    if(!b_criterion(pa)) {
      logger.config(pa.toString)
      val p = normalize(s_polynomial(polys(pa.i), polys(pa.j)).reduce(polys.toSeq*))
      if (!p.isZero) update(p)
      npairs += 1
    }
    remove(pa)
  }
  def b_criterion(pa: Pair[N]): Boolean = {
    var k = 0
    while (k < polys.size) {
      if ((k.headPowerProduct | pa.scm) && considered(pa.i, k) && considered(pa.j, k)) return true
      k += 1
    }
    false
  }
  def remove(pa: Pair[N]): Unit = {
    pairs -= pa
    if(pa.reduction) removed(pa.principal) = true
  }
  def add(pa: Pair[N]): Unit = {
    pairs += pa
    if (pa.coprime) remove(pa)
  }

  def apply(i: Int, j: Int) = {
    val m = i.headPowerProduct
    val n = j.headPowerProduct
    val scm = pp.lcm(m, n)
    new Pair(i, j, m, n, scm)
  }
  def sorted(i: Int, j: Int) = if (i > j) apply(j, i) else apply(i, j)
  def make(index: Int): Unit = for (i <- 0 until index) add(apply(i, index))
  def considered(i: Int, j: Int) = !pairs.contains(sorted(i, j))

  def ordering = Ordering by { (pair: Pair[N]) => pair.key }

  var pairs = SortedSet.empty(using ordering)
  val removed = ListBuffer.empty[Boolean]
  val polys = ListBuffer.empty[T]
  var npairs = 0
  var npolys = 0

  extension (i: Int) def headPowerProduct = polys(i).headPowerProduct

  def process(xs: Seq[T]): List[T] = {
    update(xs)
    process
    reduce
    toList
  }

  def update(s: Seq[T]): Unit = {
    logger.config(s.toList.show)
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
    logger.config("(" + index.headPowerProduct.show + ", " + index + ")")
    make(index)
    npolys += 1
  }

  def process: Unit = {
    logger.config("process")
    while(!pairs.isEmpty) process(pairs.head)
  }

  def reduce: Unit = {
    logger.config("reduce")
    for (i <- polys.size - 1 to 0 by -1 if removed(i)) {
      removed.remove(i)
      polys.remove(i)
    }
    for (i <- 0 until polys.size) {
      polys(i) = normalize(polys(i).reduce(false, true, polys.toSeq*))
      logger.config("(" + i.headPowerProduct.show + ")")
    }
  }

  def toList = {
    logger.config("statistic = (" + npairs + ", " + npolys + ", " + polys.size + ")")
    polys.toList
  }
}
