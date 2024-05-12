package vending

import SimpleTypes.{Denomination, Money, Quantity, Result}

object VendingMachineGreedyService extends VendingMachine:

  /*** Given a tuple t of (Denomination,Quantity), check the which part of Quantity can be used to create the Money on tuple r  
   * 
   * @param r A tuple (Map[Denomination,Quantity], Money), in which the map has the Quantity of Denomination 
   *          accumulated so far and the Money represents the change that is still left to give.
   * @param t a tuple (Denomination,Quantity) of how much Quantity of a specific Denomination is available to produce change.
   * @return the tuple (Map[Denomination,Quantity], Money), in which map accumulates the Quantity of Denomination that 
   *         can be used from t and the Money is subtracted from tuple r in the same amount (it must never return an error)
   */
  private def acc(r: Result[(Map[Denomination,Quantity], Money)], t: (Denomination,Quantity)): Result[(Map[Denomination,Quantity], Money)] =
    // Get the denomination and quantity from tuple t
    val (d,q) = t
    for
      // Get the map and money from tuple r
      (mr,m)    <-    r
      // calculate actual quantity aq
      aq        =     q min (m/d)
      // nm equals money m , removing as much denomination d as possible
      nm        <-    m - d.value*aq
      // If aq is zero, leave r as is, else add (Denomination,Quantity) to the map and reduce Money in the same amount
    yield (if (aq.isZero) (mr,m) else ((mr + (d->aq)), nm))



  def calculateChange(m: Map[Denomination,Quantity], change: Money): Result[Map[Denomination,Quantity]] =
    // sorted list of tuples (Denomination,Quantity) with descending Denomination order 
    val ol = m.toList.sortWith{ case ((d1,q1),(d2,q2)) => (d1.value>d2.value) }
    // Accumulate a tuple (Map[Denomination,Quantity], Money), starting with the complete Money and and empty map.
    // Folding the sorted list, trying first the greater denomination, will fill the map, reducing the money.
    val r = ol.foldLeft[Result[(Map[Denomination,Quantity], Money)]](Right(Map(),change))(acc)
    // Check if change is complete (resulting money is zero) else return DomainError
    r.fold( de => Left(de), (mp,rm) => if (rm.isZero) Right(mp) else Left(DomainError.InsuficientChange(m, change)) )
