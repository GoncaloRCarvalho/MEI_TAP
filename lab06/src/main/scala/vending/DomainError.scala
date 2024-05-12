package vending

import SimpleTypes.{Denomination, Money, Quantity}

enum DomainError:
  case InvalidMoney(i: Int)
  case InvalidQuantity(i: Int)
  case InsuficientChange(m: Map[Denomination,Quantity], c: Money)
