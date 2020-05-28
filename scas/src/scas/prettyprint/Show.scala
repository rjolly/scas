package scas.prettyprint

import scas.math.Ordering

trait Show[T] {
  def fenced(s: String) = Show.fenced(s)
  type Level = Show.Level
  val Level = Show.Level
  def (x: T).toCode(level: Level): String
  def (x: T).show = x.toCode(Level.Addition)
  def (x: T).math = Show.math(x.toMathML)
  def (x: T).toMathML: String
  def (s: List[T]).show: String = Show.listed(s.map(show(_)): _*)
  def (s: List[T]).math = Show.math(s.map(_.toMathML): _*)
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
