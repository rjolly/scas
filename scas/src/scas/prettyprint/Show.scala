package scas.prettyprint

import scas.math.Ordering

trait Show[T] {
  type Level = Show.Level
  val Level = Show.Level
  def (x: T).toCode(level: Level): String
  class Object(x: T) extends MathObject {
    override def toString = x.toCode(Level.Addition)
    def toMathML = x.toMathML
  }
  def (x: T).show: MathObject = new Object(x)
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
