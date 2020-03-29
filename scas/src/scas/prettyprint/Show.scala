package scas.prettyprint

import scas.math.Ordering

trait Show[T] {
  type Level = Show.Level
  val Level = Show.Level
  def (x: T).toCode(level: Level): String
  def (x: T).code = x.toCode(Level.Addition)
  def mml(x: T) = s"<math>${x.toMathML}</math>"
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
}
