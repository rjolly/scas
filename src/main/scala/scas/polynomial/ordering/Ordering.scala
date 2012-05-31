package scas.polynomial.ordering

trait Ordering[@specialized(Int, Long) N] extends scala.math.Ordering[Array[N]] {
  implicit val nm: scala.math.Ordering[N]
}

object Ordering {
  def lexicographic[@specialized(Int, Long) N](implicit nm: scala.math.Ordering[N]) = new Lexicographic[N]
  def degreeLexicographic[@specialized(Int, Long) N](implicit nm: scala.math.Ordering[N]) = new DegreeLexicographic[N]
  def degreeReverseLexicographic[@specialized(Int, Long) N](implicit nm: scala.math.Ordering[N]) = new DegreeReverseLexicographic[N]
  def kthElimination[@specialized(Int, Long) N](k: Int)(implicit nm: scala.math.Ordering[N]) = new KthElimination[N](k)
}
