package scas.structure

trait EuclidianDomain[@specialized(Int, Long) T] extends UniqueFactorizationDomain[T] {
  def norm(x: T): java.lang.Comparable[_]
}
