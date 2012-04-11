package scas.structure

trait EuclidianDomain[T] extends UniqueFactorizationDomain[T] {
  def norm(x: T): java.lang.Comparable[_]
}
