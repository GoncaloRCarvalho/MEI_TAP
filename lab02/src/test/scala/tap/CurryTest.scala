package tap

import org.scalatest.funsuite.AnyFunSuiteLike
import tap.Curry.{multiply_by_three, multiply_by_two}

class CurryTest extends AnyFunSuiteLike:

  test("testMultiply_by_three"):
    assert(multiply_by_three(2) == 6)
    assert(multiply_by_three(3) == 9)
    assert(multiply_by_three(4) == 12)

  test("testMultiply_by_two"):
    assert(multiply_by_two(2) == 4)
    assert(multiply_by_two(3) == 6)
    assert(multiply_by_two(4) == 8)