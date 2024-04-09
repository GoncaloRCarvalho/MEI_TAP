package tap

import SimpleTypes.*

final case class OtherStudent(firstName: String, lastName: String, age: Int)

object OtherStudent:
  private def unsafeOtherStudent(firstName: String, lastName: String, age:Int): OtherStudent = OtherStudent(firstName, lastName, age)
  def from(firstName: String, lastName: String, age:Int): Option[OtherStudent]=
    if ( SimpleTypes.StringWithOnlyLeters.from(firstName).isDefined && SimpleTypes.StringWithOnlyLeters.from(lastName).isDefined
      && SimpleTypes.PositiveInt.from(age).isDefined )
      Some(unsafeOtherStudent(firstName, lastName, age)) else None
