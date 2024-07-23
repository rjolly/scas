package scas.polynomial

trait PolynomialWithGB[T, C, M] extends PolynomialOverUFD[T, C, M] {
  def normalize(x: T) = primitivePart(x)
  def s_polynomial(x: T, y: T) = {
    val (m, a) = head(x)
    val (n, b) = head(y)
    val gcd = pp.gcd(m, n)
    val (m0, n0) = (m / gcd, n / gcd)
    x.ppMultiplyRight(n0).reduce(m0, a, y, b)
  }
  def gcd(x: T, y: T) = {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    val list = gb(p, q)
    (if (list.size == 1) list(0) else one)%* ring.gcd(a, b)
  }
  def gb(xs: T*) = {
    new Engine(using this).process(xs)
  }
}
