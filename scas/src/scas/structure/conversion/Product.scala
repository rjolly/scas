package scas.structure.conversion

import scas.structure.Product.{Element, Impl}
import scas.util.{Conversion, unary_~}

class Product[R1: scas.structure.Ring, R2: scas.structure.Ring] extends scas.structure.Product[R1, R2] with Ring[Element[R1, R2]] {
  given Product[R1, R2] = this
}

object Product extends Impl {
  def apply[R1, R2, U: Conversion[R1], V: Conversion[R2]](using factory: scas.structure.Product[R1, R2])(a: U, b: V) = factory(~a, ~b)

  override def apply[R1, R2](ring1: scas.structure.Ring[R1], ring2: scas.structure.Ring[R2]) = super.apply(ring1, ring2)
}
