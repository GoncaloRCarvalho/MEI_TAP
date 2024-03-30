package tap
object StringFunctions:
  def removeDigits(s: String): String = s.filter(!"0123456789".contains(_))
  def incDigits(s: String,i: Int) = s.map(x => if (x.isDigit) (x+i).toChar else x)