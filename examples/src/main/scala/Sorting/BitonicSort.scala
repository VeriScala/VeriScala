package NewHDLExample.Sorting.BitonicSort

import NewHDL.Core.HDLBase._

class BitonicSort[T](clk: HDL[Boolean], rst: HDL[Boolean],
  a0: HDL[T], a1: HDL[T], a2: HDL[T], a3: HDL[T],
  a4: HDL[T], a5: HDL[T], a6: HDL[T], a7: HDL[T],
  b0: HDL[T], b1: HDL[T], b2: HDL[T], b3: HDL[T],
  b4: HDL[T], b5: HDL[T], b6: HDL[T], b7: HDL[T], init: T) extends HDLClass {

  val a = List(a0, a1, a2, a3, a4, a5, a6, a7)
  val b = List(b0, b1, b2, b3, b4, b5, b6, b7)

  val DES = 0
  val ASC = 1

  def compare(a: HDL[T], b: HDL[T], x: HDL[T], y: HDL[T], dir: Int) {
    async {
      if (dir == ASC) {
        when (a > b) {
          x := b
          y := a
        } .otherwise {
          x := a
          y := b
        }
      } else {
        when (a > b) {
          x := a
          y := b
        } .otherwise {
          x := b
          y := a
        }
      }
    }
  }

  def bitonicMerge(a: List[HDL[T]], b: List[HDL[T]], dir: Int): HDLBlock = {
    val n = a.size
    val k = n / 2
    if (n > 1) {
      val t = (for (i <- 0 until n) yield HDLlize(init)).toList
      for (i <- 0 until k) {
        compare(a(i), a(i + k), t(i), t(i + k), dir)
      }
      bitonicMerge(t.take(k), b.take(k), dir)
      bitonicMerge(t.drop(k), b.drop(k), dir)
    } else {
      async {
        b.head := a.head
      }
    }
  }

  def bitonicSort(a: List[HDL[T]], b: List[HDL[T]], dir: Int): HDLBlock = {
    val n = a.size
    val k = n / 2
    if (n > 1) {
      val t = (for (i <- 0 until n) yield HDLlize(init)).toList
      bitonicSort(a.take(k), t.take(k), ASC)
      bitonicSort(a.drop(k), t.drop(k), DES)
      bitonicMerge(t, b, dir)
    } else {
      async {
        b.head := a.head
      }
    }
  }

  def sort = module {
    bitonicSort(a, b, ASC)
  }

  override val toCompile = List(sort)
}

object Main {
  def main(args: Array[String]) {
    println((new BitonicSort(b0, b1,
      Unsigned(0, 4), Unsigned(0, 4), Unsigned(0, 4), Unsigned(0, 4),
      Unsigned(0, 4), Unsigned(0, 4), Unsigned(0, 4), Unsigned(0, 4),
      Unsigned(0, 4), Unsigned(0, 4), Unsigned(0, 4), Unsigned(0, 4),
      Unsigned(0, 4), Unsigned(0, 4), Unsigned(0, 4), Unsigned(0, 4),
      Unsigned(0, 4))).compile)
  }
}
