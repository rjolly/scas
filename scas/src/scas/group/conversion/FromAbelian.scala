package scas.group.conversion

import scas.structure.AbelianGroup
import scas.structure.conversion.Monoid

class FromAbelian[T](using group: AbelianGroup[T]) extends scas.group.FromAbelian[T] with Monoid[T] {
  given instance: FromAbelian[T] = this
}
