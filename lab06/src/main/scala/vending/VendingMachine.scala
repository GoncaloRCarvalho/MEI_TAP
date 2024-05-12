package vending

import vending.SimpleTypes.*

trait VendingMachine:

  /** Calculates the change to give in terms of que Quantity of each Denomination 
   * that is available in the vending machine.
   * 
   * @param m the change in the vending machine. Each Denomination has a Quantity
   * @param change the Money the vending machine has to give as change  
   * @return Either a DomainError or a Map with the Quantity of each Denomination which adds to change
   */
  def calculateChange(m: Map[Denomination,Quantity], change: Money): Result[Map[Denomination,Quantity]]
