package scas.structure.commutative.conversion

trait UniqueFactorizationDomain[T] extends scas.structure.commutative.UniqueFactorizationDomain[T] with scas.structure.conversion.NotQuiteField[T] {
  extension[U] (x: U)(using c: U => T) {
    def % (y: T) = c(x).remainder(y)
    def /%(y: T) = c(x).divideAndRemainder(y)
    def | (y: T) = c(x).factorOf(y)
  }
  extension (x: T) {
    def % [U](y: U)(using c: U => T) = x.remainder(c(y))
    def /%[U](y: U)(using c: U => T) = x.divideAndRemainder(c(y))
    def | [U](y: U)(using c: U => T) = x.factorOf(c(y))
  }
}
