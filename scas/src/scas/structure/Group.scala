package scas.structure

import scas.structure.impl.AbelianGroup

trait Group[T] extends impl.Group[T] with NotQuiteGroup[T]

object Group {
  class FromAbelian[T](using group: AbelianGroup[T]) extends impl.Group.FromAbelian[T] with Group[T] {
    given instance: FromAbelian[T] = this
  }
}
