package tap

import scala.language.adhocExtensions

import org.scalatest.funsuite.AnyFunSuite

class lab03Test extends AnyFunSuite:
  test("hasDigit") {
    assert(lab03.hasDigit("asd1asd")===true)
    assert(lab03.hasDigit("asdasd")===false)
    assert(lab03.hasDigit(List("asd", "a1"))===true)
    assert(lab03.hasDigit(Set("asd", "a1"))===true)
    assert(lab03.hasDigit(1)===true)
    assert(lab03.hasDigit('1')===true)
    assert(lab03.hasDigit(List(Set("a", "b"), Set("a1", "d"),Set("b", "c")))===true)
    assert(lab03.hasDigit('a') ===false)
    assert(lab03.hasDigit("")===false)
    assert(lab03.hasDigit(('a',"ana"))===false)
  }

  test("max_") {
    assert(lab03.max_(List(1,2,3,4))===Some(4))
    assert(lab03.max_(List(1))===Some(1))
  }

  test("max_ with empty list") {
    assert(lab03.max_(List())===None)
  }

  test("applyF_") {
    assert(lab03.applyF_(List(19, 2, 3), List("aa", "b", "c"))(lab03.f1) === "19aa2b3c")
    assert(lab03.applyF_(List(19, 2, 3), List("aa", "b", "c"))(lab03.f2) === "1923aabc")
    assert(lab03.applyF_(List(19, 2, 3), List("aa", "b", "c"))(lab03.f3) === "3219aabc")
    assert(lab03.applyF_(List(19, 2, 3), List("aa", "b", "c"))(lab03.f4) === "3291aabc")
  }
  test("applyF_ with empty lists") {
    assert(lab03.applyF_(List(), List("aa", "b", "c"))(lab03.f1) === "")
    assert(lab03.applyF_(List(), List("aa", "b", "c"))(lab03.f2) === "aabc")
    assert(lab03.applyF_(List(), List("aa", "b", "c"))(lab03.f3) === "aabc")
    assert(lab03.applyF_(List(), List("aa", "b", "c"))(lab03.f4) === "aabc")

    assert(lab03.applyF_(List(19, 2, 3), List())(lab03.f1) === "")
    assert(lab03.applyF_(List(19, 2, 3), List())(lab03.f2) === "1923")
    assert(lab03.applyF_(List(19, 2, 3), List())(lab03.f3) === "3219")
    assert(lab03.applyF_(List(19, 2, 3), List())(lab03.f4) === "3291")

    assert(lab03.applyF_(List(), List())(lab03.f1) === "")
    assert(lab03.applyF_(List(), List())(lab03.f2) === "")
    assert(lab03.applyF_(List(), List())(lab03.f3) === "")
    assert(lab03.applyF_(List(), List())(lab03.f4) === "")
  }
