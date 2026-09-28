package scas.polynomial.gb

trait Polynomial[T, C, M] extends scas.polynomial.Polynomial[T, C, M] {
  def gb(fussy: Boolean)(xs: T*) = sugar.gb(fussy)(xs*)
  def sugar: scas.polynomial.PolynomialWithSugar[T, C, M] = new PolynomialWithSugar(using this)
}
