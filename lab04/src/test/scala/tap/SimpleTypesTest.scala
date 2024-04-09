package tap

import scala.language.adhocExtensions
import org.scalatest.funsuite.AnyFunSuite
import tap.SimpleTypes.{StringWithLettersAndSpaces, StringWithOnlyLeters}

class SimpleTypesTest extends AnyFunSuite:

  import SimpleTypes.*
  
  test("String With Only Leters positive"):
    val ls = List("Ana", "Silva", "abcde", "xpto", "", "pldpwl", "zqmwpdomqwid")
    ls.foreach( s =>
      assert( StringWithOnlyLeters.from(s).fold(false)(_.to == s) )
    )
  
  test("String With Only Leters negative"):
    val ls = List("An0a", "Sil&va", "=", "!Ana", "A na")
    ls.foreach( s =>
      assert( StringWithOnlyLeters.from(s).isEmpty )
    )

  test("String With Leters and Spaces positive"):
    val ls = List("Ana Silva", "Ana Julia Silva", "", "Manuela Silva", "Ana Rita Julia Silva")
    ls.foreach( s =>
      assert( StringWithLettersAndSpaces.from(s).fold(false)(_.to == s) )
    )
  
  test("String With Leters and Spaces negative"):
    val ls = List("An0a Silva", "Ana Sil&va", "=", "!Ana Silva", "Ana Sil?va")
    ls.foreach( s =>
      assert( StringWithLettersAndSpaces.from(s).isEmpty )
    )
