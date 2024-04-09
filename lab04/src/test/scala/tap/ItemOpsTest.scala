package tap

import scala.language.adhocExtensions
import org.scalatest.funsuite.AnyFunSuite

class ItemOpsTest extends AnyFunSuite:
  
  import ItemOps.price
  
  val description1 = "a1"
  val description2 = "a2"
  val price1 = 2.5
  val price2 = 10.0
  val discount1 = 0.2
  val discount2 = 0.0
  val mm1 = 1
  val mm3 = 3
  val a1 = Article(description1, price1)
  val a2 = Article(description2, price2)
  val b1 = Bundle("b1", discount1, List(a1,a2))
  val b2 = Bundle("b2", discount2, List(a1,a2))
  val m1 = Multiple(mm1, b1)
  val m2 = Multiple(mm3, b2)
  
  test("Article Price"):
    assert(price(a1)===price1)
    assert(price(a2)===price2)
  test("Bundle Price"):
    assert(price(b1)===(price1 + price2)*(1-discount1))
    assert(price(b2)===(price1 + price2)*(1-discount2))
  test("Multiple Price"):
    assert( price(m1) === mm1 * price(b1) )
    assert( price(m2) === mm3 * price(b2) )

