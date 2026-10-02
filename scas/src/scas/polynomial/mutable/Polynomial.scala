package scas.polynomial.mutable

trait Polynomial[T, C, M] extends scas.polynomial.Polynomial[T, C, M] {
  def unmodifiable(x: T): T
  def modifiable(x: T): T
  extension (x:T) {
    override def subtract(y:T) = unmodifiable(super.subtract(modifiable(x))(y))

    override def multiply(y:T) = {
      val r = modifiable(zero)
      for (a, b) <- y.iterator do r.subtract(a, -b, x)
      unmodifiable(r)
    }

    override def reduce(ys:T*) = unmodifiable(super.reduce(modifiable(x))(ys*))

    override def reduce(strict: Boolean, tail: Boolean, ys:T*) = unmodifiable(super.reduce(modifiable(x))(strict, tail, ys*))

    override def reduce(m: M, a: C, y: T, b: C) = unmodifiable(super.reduce(modifiable(x))(m, a, y, b))

    override def subtract(m: M, c: C, y: T) = super.subtract(x)(m, c, y)
  }
}
