package vending

import scala.language.adhocExtensions

import org.scalatest.funsuite.AnyFunSuite
import vending.SimpleTypes.{Money, Quantity, Denomination}

class SimpleTypesTest extends AnyFunSuite:
  test("Money should not be negative 01 - zero is not negative"):
    val money = 0
    val result = Money.from(money)
    
    assert(result.fold(_ => false, m => m.isZero))
  test("Money should not be negative 02 - negative gives domain error"):
    val money = -1
    val result = Money.from(money)
    
    assert(result.fold(de => de == DomainError.InvalidMoney(money), _ => false))
  test("positive Money can be constructed"):
    val money = 10
    val result = Money.from(money)
    
    assert(result.fold(de => false, m => m.to == 10))
  test("Compare Money <"):
    val moneyLess = 10
    val moneyMore = 20
    val result = 
      for
        ml <- Money.from(moneyLess)
        mm <- Money.from(moneyMore)
      yield (ml < mm) 

    assert( result.fold(de => false, b => b) )
  test("Compare Money >"):
    val moneyLess = 10
    val moneyMore = 20
    val result =
      for
      ml <- Money.from(moneyLess)
      mm <- Money.from(moneyMore)
        yield (mm > ml)

    assert( result.fold(de => false, b => b) )
  test("Math Money +"):
    val money10 = 10
    val money20 = 20
    val expected = 30
    val result =
      for
        m10 <- Money.from(money10)
        m20 <- Money.from(money20)
      yield (m10 + m20)

    assert( result.fold(de => false, m => m.to == expected) )
  test("Math Money -"):
    val money100 = 100
    val money20 = 20
    val expected = 80
    val result =
      for
        m100  <- Money.from(money100)
        m20   <- Money.from(money20)
        mdif  <- m100 - m20
      yield mdif

    assert( result.fold(de => false, m => m.to == expected) )
  test("Math Money *"):
    val money10 = 10
    val quantity5 = 5
    val expected = 50
    val result =
      for
        m10 <- Money.from(money10)
        q5 <-  Quantity.from(quantity5)
      yield (m10 * q5)

    assert( result.fold(de => false, m => m.to == expected) )
  test("Math Money /"):
    val money100 = 100
    val denomination5 = 5
    val expected = 20
    val result = 
      for
        m100 <- Money.from(money100)
      yield (m100 / Denomination.fiveCent)

    assert( result.fold(de => false, m => m.to == expected) )
  
  test("Money 0 is Zero "):
    val money0 = 0

    val result =
      for
        m0 <- Money.from(money0)
      yield (m0.isZero)

    assert( result.fold(de => false, m => m) )
  test("Money 10 is not Zero"):
    val money10 = 10

    val result =
      for
        m10 <- Money.from(money10)
      yield (m10.isZero)

    assert( result.fold(de => false, m => !m) )
  

  test("Quantity should not be negative 01 - zero is not negative"):
    val quantity = 0
    val result = Quantity.from(quantity)

    assert(result.fold(_ => false, q => q.isZero))
  test("Quantity should not be negative 02 - negative gives domain error"):
    val quantity = -1
    val result = Quantity.from(quantity)

    assert(result.fold(de => de == DomainError.InvalidQuantity(quantity), _ => false))
  test("positive Quantity can be constructed "):
    val quantity = 10
    val result = Quantity.from(quantity)

    assert(result.fold(de => false, q => q.to == 10))
  test("Quantity 0 is Zero "):
    val quantity0 = 0

    val result =
      for
        q0 <- Quantity.from(quantity0)
      yield (q0.isZero)

    assert( result.fold(de => false, q => q) )
  test("Quantity 10 is not Zero"):
    val quantity10 = 10

    val result =
      for
      q10 <- Quantity.from(quantity10)
        yield (q10.isZero)

    assert( result.fold(de => false, q => !q) )
  test("Min Quantity between 10 and 20 is 10"):
    val quantity10 = 10
    val quantity20 = 20

    val result =
      for
        q10 <- Quantity.from(quantity10)
        q20 <- Quantity.from(quantity20)
      yield (q10 min q20)

    assert( result.fold(de => false, q => q.to == quantity10) )
  test("Min Quantity between 100 and 20 is 20"):
    val quantity100 = 100
    val quantity20 = 20

    val result =
      for
        q100 <- Quantity.from(quantity100)
        q20 <- Quantity.from(quantity20)
      yield (q100 min q20)

    assert( result.fold(de => false, q => q.to == quantity20) )

