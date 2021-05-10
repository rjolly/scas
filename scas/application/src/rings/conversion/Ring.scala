package rings.conversion

import scas.util.unary_~
import BigInteger.given

trait Ring[T] extends rings.Ring[T] with scas.structure.ordered.conversion.Ring[T] {
  def characteristic = ~ring.characteristic
}
