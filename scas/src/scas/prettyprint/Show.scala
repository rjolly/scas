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
    def show: String = Show.listed(s.map(_.show): _*)
    def math = Show.math(s.map(_.toMathML): _*)
  }
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
  inline def listed(s: String*) = s"List(${s.mkString(", ")})"
  inline def list(s: String*) = s"<list>${s.mkString}</list>"
  inline def math(s: String*): String = math(list(s: _*))
  inline def math(s: String) = s"<math>$s</math>"
  inline def fenced(s: String) = s"($s)"
}
