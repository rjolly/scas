package scas.prettyprint

trait Show[T] {
  extension (x: T) {
    def show: String
    def toMathML: String
  }
}

object Show {
  given listShow: [T : Show] => Show[List[T]] {
    extension (s: List[T]) {
      def show: String = s"List(${s.show(false)})"
      def show(fenced: Boolean): String = s.map(_.show).mkString(", ")
      def toMathML: String = s"<list>${s.toMathML(false)}</list>"
      def toMathML(fenced: Boolean): String = s.map(_.toMathML).mkString
    }
  }
}
