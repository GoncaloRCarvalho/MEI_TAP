package vending

import DomainError.*
import scala.annotation.targetName

object SimpleTypes:
  
  type Result[A] = Either[DomainError,A]

  opaque type Money = Int
  object Money:
    def from(i: Int): Result[Money] = if (i<0) Left(DomainError.InvalidMoney(i)) else Right(i)
    def unit: Money = 1
    def zero: Money = 0
  extension (m: Money)
    @targetName("toMoney")
    def to: Int = m
    def <(m2: Money): Boolean = m < m2
    def >(m2: Money): Boolean = m > m2
    @targetName("plusMoney")
    def +(m2: Money): Money = m + m2
    @targetName("minusMoney")
    def -(m2: Money): Result[Money] =
      val d = m - m2
      if (d<0) Left(DomainError.InvalidMoney(d)) else Right(d)
    def *(q: Quantity): Money = m * q
    def /(d: Denomination): Quantity = m / d.value
    @targetName("isZeroMoney")
    def isZero: Boolean = m == 0.0
    @targetName("incMoney")
    def inc : Money = m + 1
    @targetName("decMoney")
    def dec : Result[Money] = if (m>0) Right(m - 1) else Left(DomainError.InvalidMoney(-1))

  opaque type Quantity = Int
  object Quantity:
    def from(i: Int): Result[Quantity] = if (i<0) Left(DomainError.InvalidQuantity(i)) else Right(i)
    def unit: Quantity = 1
    def zero: Quantity = 0
    extension (q: Quantity)
      @targetName("toQuantity")
      def to: Int = q
      @targetName("isZeroQuantity")
      def isZero: Boolean = q == 0
      infix def min(q2: Quantity): Quantity = math.min(q, q2)
      @targetName("plusQuantity")
      def +(q2: Quantity): Quantity = q + q2
      @targetName("minusQuantity")
      def -(q2: Quantity): Result[Quantity] =
        val qr = q - q2
        if (qr<0) Left(DomainError.InvalidQuantity(qr)) else Right(qr)
      @targetName("incQuantity")
      def inc: Quantity = q + 1
      @targetName("decQuantity")
      def dec: Option[Quantity] = if (q>0) Some(q - 1) else None

  enum Denomination(val value: Money):
    case oneCent extends Denomination(1)
    case twoCent extends Denomination(2)
    case fiveCent extends Denomination(5)
    case tenCent extends Denomination(10)
    case twentyCent extends Denomination(20)
    case fiftyCent extends Denomination(50)
    case oneEur extends Denomination(100)
    case twoEur extends Denomination(200)
