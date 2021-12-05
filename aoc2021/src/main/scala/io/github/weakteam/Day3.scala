package io.github.weakteam

import cats.{FlatMap, Id}
import cats.syntax.apply._
import cats.syntax.either._
import cats.instances.option._

object Day3 {

  def refine(list: List[String]): List[List[Bin]] =
    list.map(_.flatMap(Bin.unapply).toList)

  def simple(list: List[List[Bin]]) = {

    val (f, s) = list.transpose.flatMap { line =>
      val map = line.groupMapReduce(identity)(_ => 1)(_ + _)
      map.get(Bin.Zero).map2(map.get(Bin.One))((z, o) => if (z > o) (Bin.Zero, Bin.One) else (Bin.One, Bin.Zero))
    }.unzip
    Bin.toDecimal(f) * Bin.toDecimal(s)
  }

  def difficult(list: List[List[Bin]]) = {
    def findAn(f: (List[List[Bin]], List[List[Bin]]) => List[List[Bin]]) = FlatMap[Id].tailRecM((list, 0)) {
      case (list, pos) =>
        list match {
          case head :: Nil => Right(head)
          case _ =>
            val map = list.groupBy(_(pos))
            val zeros = map
              .getOrElse(Bin.Zero, Nil)
            val ones = map.getOrElse(Bin.One, Nil)
            (f(zeros, ones), pos + 1).asLeft[List[Bin]]
        }
    }

    val oxy = findAn((z, o) => if (o.size >= z.size) o else z)
    val co2 = findAn((z, o) => if (z.size <= o.size) z else o)
    Bin.toDecimal(oxy) * Bin.toDecimal(co2)
  }

  def simpleRun = refine _ andThen simple
  def difficultRun = refine _ andThen difficult

}
