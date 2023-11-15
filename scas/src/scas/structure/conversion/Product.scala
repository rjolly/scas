package scas.structure.conversion

class Product[R1 : scas.structure.Ring, R2 : scas.structure.Ring] extends scas.structure.Product[R1, R2] with Ring[(R1, R2)] {
  given instance: Product[R1, R2] = this
}
