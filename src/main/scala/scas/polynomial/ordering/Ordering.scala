package scas.polynomial.ordering

trait Ordering[@specialized(Int, Long) N] extends scala.math.Ordering[Array[N]] {
  implicit val nm: scala.math.Ordering[N]
}
