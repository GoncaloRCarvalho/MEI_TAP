package vending

import scala.language.adhocExtensions
import org.scalacheck.*
import org.scalacheck.Test.Parameters
import org.scalacheck.Prop.forAll
import vending.SimpleTypes.*

object VendingMachineServiceProperties extends Properties("VendingMachineServiceProperties"):
  // maximum quantity of any Denomination
  val MAX_QUANTITY = 50
  // vending machine service to test
  val VendingMachineService = VendingMachineHybridService

  override def overrideParameters(prms: Test.Parameters): Test.Parameters =
    prms.withMinSuccessfulTests(300)

  def genQuantity: Gen[Quantity] = for {
    q <- Gen.chooseNum(1, MAX_QUANTITY)
    quantity <- Quantity.from(q).fold(_ => Gen.fail[Quantity], Gen.const)
  } yield quantity


  def genNonEmptyDenominationList: Gen[List[Denomination]] = for {
    s <- Gen.chooseNum(1, Denomination.values.length)
    denominations <- Gen.pick(s, Denomination.values)
  } yield List.from(denominations)

  def genNonEmptyChangeMap: Gen[Map[Denomination, Quantity]] = for {
    denominations <- genNonEmptyDenominationList
    quantities <- Gen.listOfN(denominations.length, genQuantity)
  } yield denominations.zip(quantities).toMap

  def genMapAndDrop: Gen[(Map[Denomination, Quantity], Int)] =
    for {
      m <- genNonEmptyChangeMap
      d <- Gen.chooseNum(1, m.size)
    } yield (m, d)

  def mapToMoney(m: Map[Denomination, Quantity]): Money =
    m.foldLeft(Money.zero) { case (m, (d, q)) => m + d.value * q }

  def merge(m1: Map[Denomination, Quantity], m2: Map[Denomination, Quantity]): Map[Denomination, Quantity] =
    m1 ++ m2.map((d, q) => d -> (q + m1.getOrElse(d, Quantity.zero)))

  property("Exact Change") = forAll(genNonEmptyChangeMap)(m => {
    val money = mapToMoney(m)
    val opm = VendingMachineService.calculateChange(m, money)
    opm.fold(_ => false, rm => rm == m)
  })

  property("More than Change") = forAll(genNonEmptyChangeMap, genNonEmptyChangeMap)((m1, m2) => {
    val money = mapToMoney(m1)
    val opm = VendingMachineService.calculateChange(merge(m1, m2), money)
    opm.fold(_ => false, rm => mapToMoney(rm) == money)
  })

  property("Less than Change") = forAll(genMapAndDrop)((m, d) => {
    val money = mapToMoney(m)
    val opm = VendingMachineService.calculateChange(m.drop(d), money)
    opm.fold(_ => true, _ => false)
  })
