package scas.power.sparse

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.boundary, boundary.break
import scas.power.ArrayPowerProduct

trait PowerProduct extends ArrayPowerProduct[Int] {
  override def empty = Array.empty[Int]
  override def generator(n: Int) = {
    val z = ArrayBuffer.empty[(Int, Int)]
    z += ((n, 1))
    toArray(z)
  }
  override def gcd(x: Array[Int], y: Array[Int]) = {
    val z = ArrayBuffer.empty[(Int, Int)]
    for i <- 0 until x.size do {
      val j = y.indexOf(x(i))
      if j > -1 then {
        z += ((x(i), scala.math.min(x.get(i), y.get(j))))
      }
    }
    toArray(z)
  }
  override def lcm(x: Array[Int], y: Array[Int]) = {
      val z = ArrayBuffer.empty[(Int, Int)]
      var i = 0
      var j = 0
      while i < x.size && j < y.size do {
        if x(i) < y(j) then {
          z += ((x(i), x.get(i)))
          i += 1
        } else if x(i) > y(j) then {
          z += ((y(j), y.get(j)))
          j += 1
        } else {
          z += ((x(i), scala.math.max(x.get(i), y.get(j))))
          i += 1
          j += 1
        }
      }
      while i < x.size do {
        z += ((x(i), x.get(i)))
        i += 1
      }
      while j < y.size do {
        z += ((y(j), y.get(j)))
        j += 1
      }
      toArray(z)
  }
  extension (x: Array[Int]) {
    override def multiply(y: Array[Int]) = {
      val z = ArrayBuffer.empty[(Int, Int)]
      var i = 0
      var j = 0
      while i < x.size && j < y.size do {
        if x(i) < y(j) then {
          z += ((x(i), x.get(i)))
          i += 1
        } else if x(i) > y(j) then {
          z += ((y(j), y.get(j)))
          j += 1
        } else {
          z += ((x(i), x.get(i) + y.get(j)))
          i += 1
          j += 1
        }
      }
      while i < x.size do {
        z += ((x(i), x.get(i)))
        i += 1
      }
      while j < y.size do {
        z += ((y(j), y.get(j)))
        j += 1
      }
      toArray(z)
    }
    override def divide(y: Array[Int]) = {
      val z = ArrayBuffer.empty[(Int, Int)]
      for i <- 0 until x.size do {
        val j = y.indexOf(x(i))
        val c = if j > -1 then y.get(j) else 0
        assert (x.get(i) >= c)
        if x.get(i) > c then {
          z += ((x(i), x.get(i) - c))
        }
      }
      toArray(z)
    }
    override def factorOf(y: Array[Int]) = boundary {
      for i <- 0 until x.size do {
        val j = y.indexOf(x(i))
        val c = if j > -1 then y.get(j) else 0
        if x.get(i) > c then break(false)
      }
      true
    }
    override def projection(n: Int, m: Int) = {
      val z = ArrayBuffer.empty[(Int, Int)]
      for i <- 0 until x.size do {
        if x(i) >= n && x(i) < m then {
          z += ((x(i), x.get(i)))
        }
      }
      toArray(z)
    }
    override def convert(from: ArrayPowerProduct[Int]): Array[Int] = {
      val z = ArrayBuffer.empty[(Int, Int)]
      val index = from.variables.map(a => variables.indexOf(a))
      for i <- 0 until x.size do {
        val c = index(x(i))
        assert (c > -1)
        z += ((c, x.get(i)))
      }
      toArray(z.sortBy((s, _) => s))
    }
    override def dependencyOnVariables = x
    override def toCode(level: Level, times: String) = {
      var s = "1"
      var m = 0
      for i <- 0 until x.size do {
        val a = variables(x(i))
        val b = x.get(i)
        val t = if b == 1 then a.toString else s"$a\\$b"
        s = if m == 0 then t else s + times + t
        m += 1
      }
      s
    }
    override def toMathML(times: String) = {
      var s = "<cn>1</cn>"
      var m = 0
      for i <- 0 until x.size do {
        val a = variables(x(i))
        val b = x.get(i)
        val t = if b == 1 then a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
        s = if m == 0 then t else s"<apply><$times/>$s$t</apply>"
        m += 1
      }
      s
    }
    override def size = x.length >> 1
    override def deg = {
      var d = numeric.zero
      for i <- 0 until x.size do d += x.get(i)
      d
    }
    def indexOf(elem: Int): Int = {
      val n = x.indexOf(elem, 0, x.size)
      if n == x.size || x(n) > elem then -1 else n
    }
    @tailrec final def indexOf(elem: Int, i: Int, j: Int): Int = {
      val n = (i + j) >> 1
      if n == j then n else {
        val s = x(n)
        if s > elem then x.indexOf(elem, i, n)
        else if s < elem then x.indexOf(elem, n + 1, j)
        else n
      }
    }
    override def get(i: Int) = x(i + x.size)
    override def set(i: Int, c: Int) = ???
  }
  def toArray(buffer: ArrayBuffer[(Int, Int)]) = (buffer.map(_._1) ++ buffer.map(_._2)).toArray
}
