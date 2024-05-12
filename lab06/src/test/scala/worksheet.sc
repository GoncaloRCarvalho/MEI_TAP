import org.scalacheck.Gen
import vending.SimpleTypes.{Denomination, Quantity}

val MAX_QUANTITY = 50

def genQuantity: Gen[Quantity] = for {
  q <- Gen.chooseNum(1, MAX_QUANTITY)
  quantity <- Quantity.from(q).fold(_ => Gen.fail[Quantity], Gen.const)
} yield quantity

def genNonEmptyDenominationList: Gen[List[Denomination]] = for {
  s <- Gen.chooseNum(1, Denomination.values.length)
  denominations <- Gen.pick(s, Denomination.values)
} yield List.from(denominations)