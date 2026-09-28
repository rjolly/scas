package scas.polynomial.binary

import scala.compiletime.deferred
import scas.polynomial.ConvertablePolynomial
import scas.polynomial.PolynomialWithDefining
import scas.power.compact.BinaryPowerProduct
import scas.base.BigInteger.given

trait Polynomial[T, C] extends ConvertablePolynomial[T, C, Int] with PolynomialWithDefining[T, C, Array[Int]] {
  given pp: BinaryPowerProduct = deferred
  def apply(d: Int) = apply((pp.generator(d), ring.zero))
  extension (x: T) {
    def index = super.headPowerProduct(x).dependencyOnVariables(0)
    def defining = !x.isZero && x.headCoefficient.isZero
    override def headPowerProduct = {
      if x.defining then pp.defining.convert(super.headPowerProduct(x))(pp) \ 2
      else pp.defining.convert(super.headPowerProduct(x))(pp)
    }
  }
}
