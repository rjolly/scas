package scas.structure

trait Field[T] extends Field.Impl[T] with NotQuiteField[T] with NotQuiteGroup[T]

object Field {
  trait Impl[T] extends NotQuiteField.Impl[T] with NotQuiteGroup.Impl[T] {
    extension (x: T) {
      def isUnit = !x.isZero
      def divide(y: T) = x * inverse(y)
    }
  }
  def apply[T : Impl] = summon[Impl[T]]
}
