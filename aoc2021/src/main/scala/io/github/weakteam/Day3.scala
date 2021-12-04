package io.github.weakteam

import cats.syntax.apply._
import cats.instances.option._

object Day3 {

  sealed trait Bin extends Product with Serializable

  object Bin {
    def unapply(s: Char): Option[Bin] = s match {
      case '0' => Some(Zero)
      case '1' => Some(One)
      case _ => None
    }
    case object Zero extends Bin
    case object One extends Bin
  }

  def refine(list: List[String]): List[List[Bin]] = {
    list.map(_.flatMap(Bin.unapply).toList).transpose
  }

  def simple(list: List[List[Bin]]) = {
    def toDecimal(list: List[Bin]): BigInt = {
      list.foldRight((BigInt(0), 0)) { case (bin, (acc, pwr)) =>
        bin match {
          case Bin.Zero => (acc, pwr + 1)
          case Bin.One => (acc + BigInt(scala.math.pow(2, pwr).toInt), pwr + 1)
        }
      }._1
    }

    val (f, s) = list.flatMap { line =>
      val map = line.groupMapReduce(identity)(_ => 1)(_ + _)
      map.get(Bin.Zero).map2(map.get(Bin.One))((z, o) => if (z > o) (Bin.Zero, Bin.One) else (Bin.One, Bin.Zero))
    }.unzip
    toDecimal(f) * toDecimal(s)
  }

  def simpleRun = refine _ andThen simple

}
