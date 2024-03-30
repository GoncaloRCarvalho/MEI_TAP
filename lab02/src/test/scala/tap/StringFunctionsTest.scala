package tap
import scala.language.adhocExtensions
import org.scalatest.funsuite.AnyFunSuite
class StringFunctionsTest extends AnyFunSuite:
  test("removeDigits on a blank string should result in a blank string"):
    assert(StringFunctions.removeDigits("")==="")
  test("removeDigits on abc95def0ghi should result in abcdefghi "):
    assert(StringFunctions.removeDigits("abc95def0ghi")==="abcdefghi")
