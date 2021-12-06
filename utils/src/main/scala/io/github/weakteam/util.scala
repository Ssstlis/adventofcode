package io.github.weakteam

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.util.Try

object util {

  def repeatedSpan[T](iterable: List[T])(pred: T => Boolean): Vector[List[T]] = {
    @tailrec
    def loop(acc: Vector[List[T]], rest: List[T]): Vector[List[T]] =
      rest.span(pred) match {
        case (Nil, _ :: rest) => loop(acc, rest)
        case (res, _ :: rest) => loop(acc :+ res, rest)
        case (res, Nil) => acc :+ res
      }

    loop(Vector(), iterable)
  }

  def readFileLines(src: String): Try[List[String]] =
    Using(Source.fromFile(src))(_.getLines().toList)
}
