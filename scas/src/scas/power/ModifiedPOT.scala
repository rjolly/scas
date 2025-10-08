package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric

class ModifiedPOT[N : {Numeric, ClassTag}](factory: ArrayPowerProduct[N], name: String, dimension: Int) extends POT(factory, name, dimension) {
  override def compare(x: Array[N], y: Array[N]) = {
    if x(length) < y(length) then return -1
    if x(length) > y(length) then return 1
    val s = factory.compare(x, y)
    if s < 0 then return -1
    if s > 0 then return 1
    var i = 1
    while i < dimension do {
      if x(length + i) < y(length + i) then return -1
      if x(length + i) > y(length + i) then return 1
      i += 1
    }
    return 0
  }
}
