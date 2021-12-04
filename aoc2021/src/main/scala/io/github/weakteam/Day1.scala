package io.github.weakteam

object Day1 {

  def refine(list: List[String]): List[Int] = list.flatMap(_.toIntOption)

  def simple(list: List[Int]): Int = list.sliding(2).count {
    case f :: s :: Nil => f < s
    case _ => false
  }

  def difficult(list: List[Int]): Int = simple(list.sliding(3, 1).map(_.sum).toList)

  def simpleRun = refine _ andThen simple
  def difficultRun = refine _ andThen difficult
}
