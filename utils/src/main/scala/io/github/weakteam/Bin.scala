package io.github.weakteam

sealed trait Bin extends Product with Serializable {
  override def toString = this match {
    case Bin.Zero => "0"
    case Bin.One => "1"
  }
}

object Bin {
  def unapply(s: Char): Option[Bin] = s match {
    case '0' => Some(Zero)
    case '1' => Some(One)
    case _ => None
  }
  case object Zero extends Bin
  case object One extends Bin

  def toDecimal(list: List[Bin]): BigInt =
    list
      .foldRight((BigInt(0), 0)) { case (bin, (acc, pwr)) =>
        bin match {
          case Bin.Zero => (acc, pwr + 1)
          case Bin.One => (acc + BigInt(scala.math.pow(2, pwr).toInt), pwr + 1)
        }
      }
      ._1

  def show(list: List[Bin]): String = list.mkString
}
