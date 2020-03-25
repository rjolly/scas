package scas.structure

trait SemiGroup[T] extends Structure[T] {
  def (x: T) * (y: T): T
}
