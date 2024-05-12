package vending

import SimpleTypes.{Denomination, Money, Quantity, Result}

object VendingMachineHybridService extends VendingMachine:

  def calculateChange(me: Map[Denomination, Quantity], change: Money): Result[Map[Denomination, Quantity]] =
    val result = VendingMachineGreedyService.calculateChange(me, change)
    
    result.fold(
      error   => VendingMachineDPService.calculateChange(me, change),
      success => result
    )
