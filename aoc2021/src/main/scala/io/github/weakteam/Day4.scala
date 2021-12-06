package io.github.weakteam

import cats.syntax.either._
import cats.syntax.option._
import cats.{FlatMap, Id}
import io.github.weakteam.util.repeatedSpan

object Day4 {

  final case class Cell(value: Int, isMarked: Boolean = false) {
    def check(n: Int) = if (value == n) copy(isMarked = true) else this
    override def toString = s"$value${if (isMarked) 'x' else 'o'}"
  }

  final case class Line(cells: List[Cell]) {
    override def toString = s"Line(${cells.mkString(",")})"
  }

  final case class Board(lines: List[Line]) {
    def calculate(mult: Int) = lines.flatMap(_.cells).collect { case Cell(v, false) => v }.sum * mult
    override def toString = s"Board(${lines.mkString("\n")})"
  }

  final case class Data(income: List[Int], boards: List[Board])

  def refine(list: List[String]): Data =
    list match {
      case head :: _ :: boards =>
        Data(
          head.split(",").flatMap(_.toIntOption).toList,
          repeatedSpan(boards)(_.nonEmpty).toList
            .map(t => t.map(_.trim.split("\\s+").flatMap(_.toIntOption.map(Cell(_))).toList).map(Line))
            .map(Board)
        )
      case _ => Data(Nil, Nil)
    }

  def onIncome(n: Int): List[Board] => List[Board] = list =>
    list.map(b => b.copy(lines = b.lines.map(l => l.copy(cells = l.cells.map(_.check(n))))))

  def predicate: Board => Boolean = (
    board =>
      board.lines.exists(_.cells.forall(_.isMarked)) ||
        board.lines.map(_.cells).transpose.exists(_.forall(_.isMarked))
  )

  def withRun(f: (List[Board], List[Board]) => Either[List[Board], Option[Board]]) = (data: Data) => {
    val (first, rest) = data.income.splitAt(5)
    val boards = first.foldLeft(data.boards)((boards, n) => onIncome(n)(boards))
    FlatMap[Id]
      .tailRecM((boards, rest)) { case (boards, incomes) =>
        incomes match {
          case income :: rest =>
            f.tupled(onIncome(income)(boards).partition(predicate)).bimap((_, rest), _.map(_.calculate(income)))
          case _ => Right(None)
        }
      }
      .orEmpty
  }

  def simple =
    withRun {
      case (one :: _, _) => Right(Some(one))
      case (Nil, losted) => Left(losted)
    }

  def difficult =
    withRun {
      case (one :: Nil, Nil) => Right(Some(one))
      case (_, losted @ _ :: _) => Left(losted)
      case _ => Right(None)
    }

  def simpleRun = refine _ andThen simple
  def difficultRun = refine _ andThen difficult
}
