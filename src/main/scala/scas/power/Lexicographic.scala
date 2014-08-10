package scas.power

import scala.reflect.ClassTag
import scas.Variable
import scas.math.{Ordering, Numeric}
import Ordering.Implicits.infixOrderingOps

class Lexicographic[@specialized(Byte, Short, Int, Long) N](val variables: Array[Variable])(implicit val nm: Numeric[N], val m: ClassTag[N], val cm: ClassTag[Array[N]]) extends PowerProductWithDegree[N] {
  val self = this
  import variables.{length => n}
  def compare(x: Array[N], y: Array[N]): Int = {
    var i = n
    while (i > 0) {
      i -= 1
      if (x(i) < y(i)) return -1
      if (x(i) > y(i)) return 1
    }
    return 0
  }
}
