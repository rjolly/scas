package scas.polynomial

import scas.prettyprint.Level
import scas.structure.Ring
import Polynomial.Element

class Polynomial[C: Ring] extends Ring[Element[C]] with
  def apply(m: String, c: C) = Map(m -> c)
  def apply(c: C): Element[C] = this("one", c)
  def (x: Element[C]) + (y: Element[C]) = x ++ y
  def (x: Element[C]) - (y: Element[C]) = x ++ y.map((m, c) => (m, -c))
  def (x: Element[C]) * (y: Element[C]) = for ((m, c) <- x; (n, d) <- y) yield (m + n, c * d)
  def equiv(x: Element[C], y: Element[C]) = x == y
  def characteristic = Ring[C].characteristic
  def (x: Element[C]).isUnit = abs(x) >< one
  def signum(x: Element[C]) = x.head match { case _ -> c => Ring[C].signum(c) }
  def (x: Element[C]).toCode(level: Level) = x.toString
  def (x: Element[C]).toMathML = ???
  def zero = this("one", Ring[C].zero)
  def one = this("one", Ring[C].one)

object Polynomial with
  type Element[C] = Map[String, C]
