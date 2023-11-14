package scas.polynomial

import java.util.TreeMap
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.math.Ordering
import scas.power.PowerProduct
import scas.prettyprint.Show

trait SolvablePolynomial[T, C, M](using pp: PowerProduct[M]) extends Polynomial[T, C, M] {
  import pp.dependencyOnVariables
  type Key = (Int, Int)
  case class Relation(e: M, f: M, p: T) {
    override def toString = s"${p.show}-${e.show}*${f.show}"
    def toMathML = s"<apply><minus/>${p.toMathML}<apply><times/>${e.toMathML}${f.toMathML}</apply></apply>"
  }
  object Relation {
    given Show[Relation] with {
      extension (x: Relation) {
        def show = x.toString
        def toMathML = x.toMathML
      }
    }
  }
  val table = new TreeMap[Key, List[Relation]](Ordering[Key])
  def update(e: T, f: T, p: T): Unit = update(headPowerProduct(e), headPowerProduct(f), p)
  def update(e: M, f: M, p: T) = {
    val key = makeKey(e, f)
    val list = table.asScala.getOrElse(key, Nil)
    table.put(key, insert(list, Relation(e, f, p)))
  }
  def insert(list: List[Relation], relation: Relation): List[Relation] = list match {
    case head::tail if (factorOf(relation, head)) => head::insert(tail, relation)
    case _ => relation::list
  }
  def lookup(e: M, f: M): Relation = {
    val key = makeKey(e, f)
    val list = table.asScala.getOrElse(key, Nil)
    list match {
      case Nil => Relation(pp.one, pp.one, this(e * f))
      case _ => {
        val Relation(e0, f0, p0) = select(list, Relation(e, f, zero))
        Relation(e / e0, f / f0, p0)
      }
    }
  }
  def select(list: List[Relation], relation: Relation): Relation = list match {
    case head::tail => if (factorOf(head, relation)) head else select(tail, relation)
    case _ => relation
  }
  def factorOf(x: Relation, y: Relation) = {
    val Relation(ex, fx, px) = x
    val Relation(ey, fy, py) = y
    (ex | ey) && (fx | fy)
  }
  def makeKey(e: M, f: M) = {
    val de = dependencyOnVariables(e)
    val df = dependencyOnVariables(f)
    (de(0), df(0))
  }
  def toList = (for ((a, b) <- table.asScala; relation <- b) yield relation).toList
  override def toString = s"${super.toString}${toList.show(true)}"
  override def toMathML = s"<msub>${super.toMathML}${toList.toMathML}</msub>"

  extension (ring: Polynomial[T, C, M]) def apply(s: T*): SolvablePolynomial[T, C, M] = {
    assert (s.foldLeft(true)((l, r) => l && r.isZero))
    this
  }

  extension (x: T) {
    final override def multiply(m: M, c: C) = x.ppMultiplyRight(m)%* c

    final override def ppMultiplyRight(m: M) = iterator(x).foldLeft(zero) { (l, r) =>
      val (s, _) = r
      l + s.ppMultiply(m)
    }
  }

  extension (e: M) def ppMultiply(f: M): T = {
    val ep = dependencyOnVariables(e)
    val fp = dependencyOnVariables(f)
    if (ep.length == 0 || fp.length == 0) this(e * f) else {
      val el = ep(ep.length-1)
      val fl = fp(0)
      if (el <= fl) this(e * f) else {
        val e2 = e.projection(el)
        val f2 = f.projection(fl)
        val e1 = e / e2
        val f1 = f / f2
        val Relation(e3, f3, c3) = lookup(e2, f2)
        var cs = c3
        if (!(f3.isOne)) {
          cs = cs.ppMultiplyRight(f3)
          update(e2 / e3, f2, cs)
        }
        if (!(e3.isOne)) {
          cs = this(e3) * cs
          update(e2, f2, cs)
        }
        if (!(f1.isOne)) cs = cs.ppMultiplyRight(f1)
        if (!(e1.isOne)) cs = this(e1) * cs
        cs
      }
    }
  }
}
