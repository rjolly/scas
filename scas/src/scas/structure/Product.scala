package scas.structure

import scas.util.{Conversion, unary_~}
import scas.base.BigInteger
import BigInteger.given

class Product[R1 : impl.Ring, R2 : impl.Ring] extends impl.Product[R1, R2] with Ring[(R1, R2)] {
  given instance: Product[R1, R2] = this
}

object Product {
  def apply[R1, R2, U : Conversion[R1], V : Conversion[R2]](using factory: impl.Product[R1, R2])(a: U, b: V) = factory(~a, ~b)

  def apply[R1, R2](ring1: impl.Ring[R1], ring2: impl.Ring[R2]) = new Product(using ring1, ring2)
}
