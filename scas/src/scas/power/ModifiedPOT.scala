package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric

class ModifiedPOT[N : Numeric : ClassTag](factory: ArrayPowerProduct[N], name: String, dimension: Int) extends POT(factory, name, dimension) {
  override def compare(x: Array[N], y: Array[N]) = {
    if (x(length + 1) < y(length + 1)) return -1
    if (x(length + 1) > y(length + 1)) return 1
    val s = factory.compare(x, y)
    if (s < 0) return -1
    if (s > 0) return 1
    var i = 1
    while (i < dimension) {
      if (x(length + 1 + i) < y(length + 1 + i)) return -1
      if (x(length + 1 + i) > y(length + 1 + i)) return 1
      i += 1
    }
    return 0
  }
}
