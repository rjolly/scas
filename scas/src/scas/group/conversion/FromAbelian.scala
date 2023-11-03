package scas.group.conversion

import scas.structure.AbelianGroup
import scas.structure.conversion.Group

class FromAbelian[T](using group: AbelianGroup[T]) extends scas.group.FromAbelian[T] with Group[T] {
  given instance: FromAbelian[T] = this
}
