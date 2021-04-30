package scas.structure.conversion

import scas.structure.Product.Element
import scas.util.{Conversion, unary_~}

class Product[R1: scas.structure.Ring, R2: scas.structure.Ring] extends scas.structure.Product[R1, R2] with Ring[Element[R1, R2]] {
  given Product[R1, R2] = this
}

object Product {
  def apply[R1, R2, U: Conversion[R1], V: Conversion[R2]](using factory: scas.structure.Product[R1, R2])(a: U, b: V) = factory(~a, ~b)

  def apply[R1, R2](ring1: scas.structure.Ring[R1], ring2: scas.structure.Ring[R2]) = new Product(using ring1, ring2)
}
