package scas.power

import scas.variable.Variable

class ModifiedPOT[M : PowerProduct](using ArrayPowerProduct[Int]) extends POT[M] {
  def this(factory: PowerProduct[M])(variables: Variable*) = this(using factory, new Lexicographic[Int](variables*))
  def this(factory: PowerProduct[M])(name: String, dimension: Int) = this(factory)((for i <- 0 until dimension yield Variable(name, 0, i))*)
  override def compare(x: (M, Array[Int]), y: (M, Array[Int])) = {
    val (a, b) = x
    val (c, d) = y
    var i = 0
    if b.get(i) < d.get(i) then return -1
    if b.get(i) > d.get(i) then return 1
    val s = factory1.compare(a, c)
    if s < 0 then return -1
    if s > 0 then return 1
    i += 1
    while i < factory2.nbvars do {
      if b.get(i) < d.get(i) then return -1
      if b.get(i) > d.get(i) then return 1
      i += 1
    }
    return 0
  }
}
