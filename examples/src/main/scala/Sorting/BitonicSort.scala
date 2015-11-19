package NewHDLExample.Sorting.BitonicSort

import NewHDL.Core.HDLBase._

class BitonicSort[T](clk: HDL[Boolean], rst: HDL[Boolean],
  a: List[HDL[T]], b: List[HDL[T]], init: T) extends HDLClass {

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
    new BitonicSort[Unsigned](b0, b1,
      (0 until 8).map(_ => HDL(Unsigned(0, 4))).toList,
      (0 until 8).map(_ => HDL(Unsigned(0, 4))).toList,
      Unsigned(0, 4)).compile.toConsole
  }
}
