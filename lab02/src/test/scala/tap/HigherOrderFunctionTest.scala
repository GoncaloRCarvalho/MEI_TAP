package tap
import scala.language.adhocExtensions
import org.scalatest.funsuite.AnyFunSuite
import HigherOrderFunction.*
class HigherOrderFunctionTest extends AnyFunSuite:
  test("testIsPrime"):
    val l = (1 to 30).filter(isPrime(_))
    assert( l === List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29) )
  test("testHof"):
    val l = (1 to 5).toList
    assert( hof( isEven, square, l ) === List(4, 16) )
    assert( hof( isOdd, cube, l ) === List(1, 27, 125) )
    assert( hof(isPrime, square, l ) === List(4, 9, 25) )
  test("isEven"):
    assert(( 1 to 15 ).filter( _ % 2 == 0 ) === List(2, 4, 6, 8, 10, 12, 14))
  test("isOdd"):
    assert(( 1 to 15 ).filter( _ % 2 != 0 ) === List(1, 3, 5, 7, 9, 11, 13, 15))
  test("square") {
    assert(( 1 to 6 ).map( x => x * x ) === List( 1, 4, 9, 16, 25, 36))
  }