package scas.structure

trait Group[T] extends NotQuiteGroup[T] {
  extension (x: T) {
    def isUnit = true
  }
}

object Group {
  def apply[T](group: AbelianGroup[T]) = new scas.group.conversion.FromAbelian(using group)
}
