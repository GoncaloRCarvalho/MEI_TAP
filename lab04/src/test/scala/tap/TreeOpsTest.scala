package tap

import scala.language.adhocExtensions
import org.scalatest.funsuite.AnyFunSuite

class TreeOpsTest extends AnyFunSuite:

  import TreeOps.*

  val v5 = 5
  val v3 = 3
  val v7 = 7
  val v4 = 4
  
  val tr1 = insert(EmptyTree, v5)
  val tr2 = insert(tr1, v3)
  val tr3 = insert(tr2, v7)
  val tr4 = insert(tr3, v4)

  test("Insert on Empty tree"):
    assert( tr1 === Node(v5,EmptyTree,EmptyTree) )
  test("Insert on a tree with one element"):
    assert( tr2 === Node(v5,Node(v3,EmptyTree,EmptyTree),EmptyTree) )
  test("Insert on a tree with two elements"):
    assert( tr3 === Node(v5,Node(v3,EmptyTree,EmptyTree),Node(v7,EmptyTree,EmptyTree)) )
  test("Insert on a tree with three elements") {
    assert( tr4 === Node(v5,Node(v3,EmptyTree,Node(v4,EmptyTree,EmptyTree)),Node(v7,EmptyTree,EmptyTree)) )
  }