package scas.structure.conversion

import scas.structure.AbelianGroup

trait Group[T] extends scas.structure.Group[T] with NotQuiteGroup[T]

object Group {
  class FromAbelian[T](using group: AbelianGroup[T]) extends scas.structure.Group.FromAbelian[T] with Group[T] {
    given instance: FromAbelian[T] = this
  }
}
