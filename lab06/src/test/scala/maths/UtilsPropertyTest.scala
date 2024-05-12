package maths

import scala.language.adhocExtensions
import Utils.*
import org.scalacheck.Properties
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll

object UtilsPropertyTest extends Properties("Utils"):

  val g: Gen[Int] = Gen.choose(1, 100000)

  property("no even number greater than 2 is prime") = forAll((n: Int) =>
    if ((n > 2) && (n % 2 == 0)) !isPrime(n) else true
  )

  property("Factorial of a number multiplied by the next number is the factorial of the next number") = forAll(g):
    n => if (1 < n  && n < 7) factorial(n) * (n + 1) == factorial(n + 1) else true

  property("Factorial of a greater number is greater than the factorial of a smaller number") = forAll(g, g):
    (n1, n2) => if (1 < n1 && n1 < 7 && 1 < n2 && n2 < 7 && n1 > n2) factorial(n1) > factorial(n2) else true