package scas.polynomial

trait PolynomialWithDefining[T, C, M] extends Polynomial[T, C, M] {
  override def normalize(x: T) = {
    if (x.defining) then x
    else super.normalize(x)
  }
  override def s_polynomial(x: T, y: T) = {
    if x.defining then {
      if y.defining then ???
      else generator(x.index) * y
    } else {
      if y.defining then x * generator(y.index)
      else super.s_polynomial(x, y)
    }
  }
  extension (x: T) {
    def index: Int
    def defining: Boolean
    override def reduce(ys: T*) = {
      if x.defining then x
      else super.reduce(x)(ys.filterNot(_.defining)*)
    }
    override def reduce(strict: Boolean, tail: Boolean, ys: T*) = {
      if x.defining then x
      else super.reduce(x)(strict, tail, ys.filterNot(_.defining)*)
    }
  }
}
