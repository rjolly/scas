package scas.structure.conversion

import scas.structure.Ring

class Product[R1 : scas.structure.impl.Ring, R2 : scas.structure.impl.Ring] extends scas.structure.Product[R1, R2] with Ring[(R1, R2)] {
  given instance: Product[R1, R2] = this
}
