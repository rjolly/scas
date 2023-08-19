package scas.prettyprint

import scas.math.Ordering

enum Level {
  case Addition, Multiplication, Power
}

object Level {
  given Ordering[Level] with {
    def compare(x: Level, y: Level) = java.lang.Integer.compare(x.ordinal, y.ordinal)
  }
}
