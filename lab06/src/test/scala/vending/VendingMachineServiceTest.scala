package vending

import scala.language.adhocExtensions

import org.scalatest.funsuite.AnyFunSuite
import vending.SimpleTypes.*
import vending.SimpleTypes.Denomination.*

class VendingMachineServiceTest extends AnyFunSuite:
  // vending machine service to test
  val VendingMachineService = VendingMachineHybridService
  
  test("Change is correct 01"):
    val changeToGive = 135
    val result =
      for {
        q <- Quantity.from(1)
        m = Map(oneCent -> q, twoCent -> q, fiveCent -> q, tenCent -> q, twentyCent -> q, fiftyCent -> q, oneEur -> q, twoEur -> q )
        c <- Money.from(changeToGive)
        mc <- VendingMachineService.calculateChange(m,c)
      }
      yield mc
    val expected =
      for {
        q <- Quantity.from(1)
        m = Map(oneEur -> q, twentyCent -> q,  tenCent -> q, fiveCent -> q)
      }
      yield m
    assert(result == expected)
  test("Change is correct 02"):
    val changeToGive = 65
    val result =
      for {
        q1 <- Quantity.from(1)
        q3 <- Quantity.from(3)
        m = Map(oneCent -> q1, twoCent -> q1, fiveCent -> q1, tenCent -> q1, twentyCent -> q3, oneEur -> q1, twoEur -> q1 )
        c <- Money.from(changeToGive)
        mc <- VendingMachineService.calculateChange(m,c)
      }
      yield mc
    val expected =
      for {
        q1 <- Quantity.from(1)
        q3 <- Quantity.from(3)
        m = Map(twentyCent -> q3, fiveCent -> q1)
      }
      yield m  
    assert(result == expected)
  test("Change is correct 03"):
    val changeToGive = 99
    val result =
      for {
        q <- Quantity.from(5)
        m = Map(oneCent -> q, twoCent -> q, fiveCent -> q, tenCent -> q, twentyCent -> q, fiftyCent -> q, oneEur -> q, twoEur -> q )
        c <- Money.from(changeToGive)
        mc <- VendingMachineService.calculateChange(m,c)
      }
      yield mc
    val expected =
      for {
        q1 <- Quantity.from(1)
        q2 <- Quantity.from(2)
        m = Map(fiftyCent -> q1, twentyCent -> q2, fiveCent -> q1, twoCent -> q2)
      }
      yield m
    assert(result == expected)
  test("Change is correct 04 - zero change can always be given"):
    val changeToGive = 0
    val result =
      for {
        c <- Money.from(changeToGive)
        m = Map[Denomination,Quantity]()
        mc <- VendingMachineService.calculateChange(m,c)
      }
      yield mc
    val expected = Right(Map[Denomination,Quantity]())
    assert(result == expected)
  test("Change is not possible 01 - missing one cent"):
    val changeToGive = 136
    val mr =
      for {
        q <- Quantity.from(1)
        m = Map(fiveCent -> q, tenCent -> q, twentyCent -> q, oneEur -> q )
      }
      yield m    
    val result =
      for {
        m <- mr
        c <- Money.from(changeToGive)
        mc <- VendingMachineService.calculateChange(m,c)
      }
      yield mc
    val expected =
      (for { 
        m <- mr
        c <- Money.from(changeToGive)
      }
      yield DomainError.InsuficientChange(m,c)).fold(de => Left(de), de => Left(de))
    assert(result == expected)
  test("Change is not possible 02 - empty change map"):
    val changeToGive = 135
    val result =
      for {
        c <- Money.from(changeToGive)
        m = Map[Denomination,Quantity]()
        mc <- VendingMachineService.calculateChange(m,c)
      }
      yield mc
    val expected = Left(Money.from(changeToGive).fold(de => de, c => DomainError.InsuficientChange(Map[Denomination,Quantity](),c)))
    assert(result == expected)
  test("Change is not possible 03 - missing 5 cents"):
    val changeToGive = 135
    val mr =
      for {
        q <- Quantity.from(1)
        m = Map(oneCent -> q, twoCent -> q, tenCent -> q, twentyCent -> q, fiftyCent -> q, oneEur -> q, twoEur -> q )
      }
      yield m
    val result =
      for {
        m <- mr
        c <- Money.from(changeToGive)
        mc <- VendingMachineService.calculateChange(m,c)
      }
      yield mc
    val expected =
      (for {
        m <- mr
        c <- Money.from(changeToGive)
      }
      yield DomainError.InsuficientChange(m,c)).fold(de => Left(de), de => Left(de))
    assert(result == expected)
