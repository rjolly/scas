package scas.polynomial

import scala.collection.JavaConverters.{mapAsJavaMapConverter, mapAsScalaMapConverter}
import scas.prettyprint.Level
import scas.structure.Ring
import Polynomial.Element

class Polynomial[C: Ring] extends Ring[Element[C]] with
  def apply(m: String, c: C) = Map(m -> c).asJava
  def apply(c: C): Element[C] = this("one", c)
  def (x: Element[C]) + (y: Element[C]) = (x.asScala ++ y.asScala).asJava
  def (x: Element[C]) - (y: Element[C]) = (x.asScala ++ y.asScala.map((m, c) => (m, -c))).asJava
  def (x: Element[C]) * (y: Element[C]) = (for ((m, c) <- x.asScala; (n, d) <- y.asScala) yield (m + n, c * d)).asJava
  def equiv(x: Element[C], y: Element[C]) = x == y
  def characteristic = Ring[C].characteristic
  def (x: Element[C]).isUnit = abs(x) >< one
  def signum(x: Element[C]) = x.asScala.head match { case _ -> c => Ring[C].signum(c) }
  def (x: Element[C]).toCode(level: Level) = x.toString
  def (x: Element[C]).toMathML = ???
  def zero = this("one", Ring[C].zero)
  def one = this("one", Ring[C].one)

object Polynomial with
  type Element[C] = java.util.Map[String, C]
