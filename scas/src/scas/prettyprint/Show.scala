package scas.prettyprint

import scas.math.Ordering

trait Show[T] {
  def fenced(s: String) = Show.fenced(s)
  type Level = Show.Level
  val Level = Show.Level
  extension (x: T) {
    def toCode(level: Level): String
    def show = x.toCode(Level.Addition)
    def math = Show.math(x.toMathML)
    def toMathML: String
  }
  extension (s: List[T]) {
    def show: String = s"List(${s.map(_.show).mkString(", ")})"
    def math = Show.math(s"<list>${s.map(_.toMathML).mkString}</list>")
  }
}

object Show {
  enum Level {
    case Addition, Multiplication, Power
  }
  object Level {
    given Ordering[Level] with {
      def compare(x: Level, y: Level) = java.lang.Integer.compare(x.ordinal, y.ordinal)
    }
  }
  inline def math(s: String) = s"<math>$s</math>"
  inline def fenced(s: String) = s"($s)"
}
