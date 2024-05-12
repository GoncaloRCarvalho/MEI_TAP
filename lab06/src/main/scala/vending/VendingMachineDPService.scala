package vending

import scala.annotation.tailrec
import SimpleTypes.{Denomination, Money, Quantity, Result}

object VendingMachineDPService extends VendingMachine:
  
  /*** Transfer one of Denomination d, from Map mr to Map mg, increasing the tuple's denomination count
   * 
   * @param d   Denomination to transfer
   * @param mg  Map of Change
   * @param i   Quantity of Denomination in mg
   * @param mr  Map of Remaining Denomination
   * @return    The tuple (Map of Change, with one additional Denomination d ; Quantity of Denomination in the Map of Change + 1 ; Map of Remaining Denomination after giving change, with one Denomination d removed)
   */
  def transferOneDenomination(d: Denomination, mg: Map[Denomination, Quantity], i: Int, mr: Map[Denomination, Quantity]): Option[(Map[Denomination, Quantity],Int,Map[Denomination, Quantity])] =
    for
      qr  <- mr.get(d)
      qrr <- qr.dec
    yield (mg + (d -> mg.getOrElse(d,Quantity.zero).inc), i + 1, if (qrr.isZero) (mr - d) else (mr + (d -> qrr)))
  
  /*** Choose the one denomination that allows the creation of change, given previously calculated changes.
   * 
   * @param change  The Money that is to be given as change
   * @param ld      The List of possible Denomination a priori
   * @param mdp     The Map which will accumulate the changes for each Money. Key: Money ; Value: Map of Change ; Quantity of Denomination in the Map of Change ; Map of Remaining Denomination after giving change
   * @return        The tuple (Map of Change ; Quantity of Denomination in the Map of Change ; Map of Remaining Denomination after giving change) for given change, if it exists 
   */
  def oneDenomination(change: Money, ld: List[Denomination], mdp: Map[Money, (Map[Denomination, Quantity],Int,Map[Denomination, Quantity])] ): Option[(Map[Denomination, Quantity],Int,Map[Denomination, Quantity])] =   
    val mc = ld.flatMap( d =>
      (change - d.value).fold(_ => None, mdif => mdp.get(mdif).fold(None)( (mg,i,mr) => transferOneDenomination(d, mg, i, mr) ) )
    )
    mc.drop(1).foldLeft(mc.headOption)( (oa, i) => oa.map( a => if (i._2 < a._2) i else a ) )
  
  /*** Fill the map increasingly adding at most one coin per new solution, minimizing the number of coins in each solution.
   * 
   * @param m         The Money from which to calculate change
   * @param change    The maximum Money for which to calculate change
   * @param ld        The List of possible Denomination a priori
   * @param mdp       The Map which will accumulate the changes for each Money. Key: Money ; Value: Map of Change ; Quantity of Denomination in the Map of Change ; Map of Remaining Denomination after giving change 
   * @return          The Map mdp, filled in the range (1 to change)
   */
  @tailrec
  final def fillMap(m: Money, change: Money, ld: List[Denomination], mdp: Map[Money, (Map[Denomination, Quantity], Int, Map[Denomination, Quantity])]): Map[Money,(Map[Denomination, Quantity], Int, Map[Denomination, Quantity])] =
    if (m > change) mdp else
      oneDenomination(m, ld, mdp) match
        case None                =>  fillMap(m.inc, change, ld, mdp)
        case Some(mg,i,mr)       =>  fillMap(m.inc, change, ld, mdp + (m -> (mg,i,mr)) )
        
  /*** Fill the map, return the map value at the needed change
   *
   * @param me      The Map which represents all the Denomination inside the Vending Machine
   * @param change  The Money that is to be given as change
   * @return        The Map of the change to be given, if possible
   */
  def calculateChange(me: Map[Denomination,Quantity], change: Money): Result[Map[Denomination,Quantity]] =
    val ld = me.map( (d, _) => d).toList
    val mdpif = fillMap(Money.unit, change, ld, Map(Money.zero -> (Map(), 0, me)))
    mdpif.get(change).fold(Left(DomainError.InsuficientChange(me,change)))( (m, _, _) => Right(m) )