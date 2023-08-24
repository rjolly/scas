package scas.structure.ordered

trait AbelianGroup[T] extends AbelianGroup.Impl[T] with scas.structure.AbelianGroup[T] with Structure[T]

object AbelianGroup {
  trait Impl[T] extends scas.structure.AbelianGroup.Impl[T] with Structure.Impl[T] {
    extension (x: T) def signum = if (x < zero) -1 else if (x > zero) 1 else 0
  }
}
