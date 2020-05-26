package scas.prettyprint

import scas.math.Ordering

trait Show[T] {
  type Level = Show.Level
  val Level = Show.Level
  def (x: T).toCode(level: Level): String
  def (x: T).show = x.toCode(Level.Addition)
  def (x: T).math = Show.math(x.toMathML)
  def (x: T).toMathML: String
}

object Show {
  enum Level {
    case Addition, Multiplication, Power
  }
  object Level {
    given Ordering[Level] {
      def compare(x: Level, y: Level) = java.lang.Integer.compare(x.ordinal, y.ordinal)
    }
  }
  def math(s: String) = s"<math>$s</math>"
}
