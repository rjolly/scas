package scas.polynomial

import scala.collection.SortedMap
import scas.Implicits.{infixRingOps, infixPowerProductOps}
import Polynomial.Element

trait SolvablePolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] {
  type Key = (Int, Int)
  type Relation = (Array[N], Array[N], T)
  var table = SortedMap.empty[Key, List[Relation]]
  def update(e: T, f: T, p: T): Unit = update(headPowerProduct(e), headPowerProduct(f), p)
  def update(e: Array[N], f: Array[N], p: T) = {
    val key = makeKey(e, f)
    val list = table.getOrElse(key, Nil)
    table = table.updated(key, insert(list, (e, f, p)))
  }
  def insert(list: List[Relation], relation: Relation): List[Relation] = list match {
    case head::tail if (factorOf(relation, head)) => head::insert(tail, relation)
    case _ => relation::list
  }
  def lookup(e: Array[N], f: Array[N]): Relation = {
    val key = makeKey(e, f)
    val list = table.getOrElse(key, Nil)
    list match {
      case Nil => (pp.one, pp.one, fromPowerProduct(e * f))
      case _ => {
        val (e0, f0, p0) = select(list, (e, f, zero))
        (e / e0, f / f0, p0)
      }
    }
  }
  def select(list: List[Relation], relation: Relation): Relation = list match {
    case head::tail => if (factorOf(head, relation)) head else select(tail, relation)
    case _ => relation
  }
  def factorOf(x: Relation, y: Relation) = {
    val (ex, fx, px) = x
    val (ey, fy, py) = y
    (ex | ey) && (fx | fy)
  }
  def makeKey(e: Array[N], f: Array[N]) = {
    val de = pp.dependencyOnVariables(e)
    val df = pp.dependencyOnVariables(f)
    (de(0), df(0))
  }
  override def toString = super.toString + "[" + (for ((a, b) <- table) yield "[" + (for ((e, f, p) <- b) yield e.toCode(0) + "*" + f.toCode(0) + " = " + p.toString).mkString(", ") + "]").mkString(", ")+ "]"
  override def toMathML = <mrow>{super.toMathML}<list>{for ((a, b) <- table) yield {<list>{for ((e, f, p) <- b) yield <apply><eq/><apply><times/>{e.toMathML}{f.toMathML}</apply>{p.toMathML}</apply>}</list>}}</list></mrow>

  override def multiply(w: T, x: Array[N], y: C) = (zero /: iterator(w)) { (l, r) =>
    val (a, b) = r
    val c = b * y
    if (c.isZero) l else l + multiply(multiply(a, x), c)
  }

  def multiply(e: Array[N], f: Array[N]) = {
    val ep = pp.dependencyOnVariables(e)
    val fp = pp.dependencyOnVariables(f)
    if (ep.length == 0 || fp.length == 0) fromPowerProduct(e * f) else {
      val el = ep(ep.length-1)
      val fl = fp(0)
      if (el <= fl) fromPowerProduct(e * f) else {
        val e2 = pp.projection(e, el)
        val f2 = pp.projection(f, fl)
        val e1 = e / e2
        val f1 = f / f2
        val (e3, f3, c3) = lookup(e2, f2)
        var cs = c3
        if (!(f3.isOne)) {
          cs = cs * fromPowerProduct(f3)
          update(e2 / e3, f2, cs)
        }
        if (!(e3.isOne)) {
          cs = fromPowerProduct(e3) * cs
          update(e2, f2, cs)
        }
        if (!(f1.isOne)) cs = cs * fromPowerProduct(f1)
        if (!(e1.isOne)) cs = fromPowerProduct(e1) * cs
        cs
      }
    }
  }
}
