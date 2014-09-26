package NewHDL.Test.CoreTest

import org.scalatest.FunSuite

import NewHDL.Core.HDLBase._
import NewHDL.Exceptions._

class BoolTest extends FunSuite {
  test("test if a number if a valid bool register") {
    assert(Bool(0).value === 0)
    assert(Bool(1).value === 1)
    intercept[IllegalArgumentException] {
      Bool(-1)
    }
    intercept[NotEnoughBitsException] {
      Bool(2)
    }
  }
}

class UnsignedTest extends FunSuite {
  test("test if a number is a valid signed register") {
    assert(Unsigned(0, 1).value === 0)
    assert(Unsigned(1, 1).value === 1)
    assert(Unsigned(5, 3).value === 5)
    intercept[IllegalArgumentException] {
      Unsigned(-1, 1)
    }
    intercept[NotEnoughBitsException] {
      Unsigned(0, 0)
    }
    intercept[NotEnoughBitsException] {
      Unsigned(5, 1)
    }
    intercept[NotEnoughBitsException] {
      Unsigned(32, 4)
    }
  }
}

class SignedTest extends FunSuite {
  test("test if a number is a valid unsigned register") {
    assert(Signed(0, 2).value === 0)
    assert(Signed(1, 2).value === 1)
    assert(Signed(5, 4).value === 5)
    assert(Signed(-5, 4).value === -5)
    assert(Signed(-1, 2).value === -1)
    intercept[NotEnoughBitsException] {
      Signed(-1, 1)
    }
    intercept[NotEnoughBitsException] {
      Signed(0, 0)
    }
    intercept[NotEnoughBitsException] {
      Signed( 5, 1)
    }
    intercept[NotEnoughBitsException] {
      Signed(32, 5)
    }
  }
}
