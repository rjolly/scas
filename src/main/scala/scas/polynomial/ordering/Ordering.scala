package scas.polynomial.ordering

trait Ordering[N] extends scala.math.Ordering[Array[N]] {
  implicit val nm: scala.math.Ordering[N]
}
