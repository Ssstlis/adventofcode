package io.github.weakteam


object Day2 {

  sealed trait Move extends Product with Serializable

  object Move {
    def unapply(s: String): Option[Move] = s match {
      case s"forward $n" => n.toIntOption.map(Fwd)
      case s"up $n" => n.toIntOption.map(Up)
      case s"down $n" => n.toIntOption.map(Down)
      case _ => None
    }

    final case class Fwd(n: Int) extends Move
    final case class Up(n: Int) extends Move
    final case class Down(n: Int) extends Move
  }

  def refine(list: List[String]): List[Move] = list.flatMap(Move.unapply)

  def simple(list: List[Move]) = {
    val (fwd, dep) = list.foldLeft((0, 0)) { case ((fwd, dep), move) =>
      move match {
        case Move.Fwd(n) => (fwd + n, dep)
        case Move.Up(n) => (fwd, dep - n)
        case Move.Down(n) => (fwd, dep + n)
      }
    }
    fwd * dep
  }

  def difficult(list: List[Move]) = {
    val (fwd, dep, _) = list.foldLeft((0, 0, 0)) { case ((fwd, dep, aim), move) =>
      move match {
        case Move.Fwd(n) => (fwd + n, if (aim == 0) dep else dep + (n * aim) , aim)
        case Move.Up(n) => (fwd, dep, aim - n)
        case Move.Down(n) => (fwd, dep, aim + n)
      }
    }
    fwd * dep
  }

  def simpleRun = refine _ andThen simple

  def difficultRun = refine _ andThen difficult

}
