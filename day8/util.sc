import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.util.Try

def repeatedSpan[T](iterable: Iterable[T])(pred: T => Boolean): Iterable[Iterable[T]] = {
  @tailrec
  def loop(acс: Vector[Iterable[T]], rest: Iterable[T]): Iterable[Iterable[T]] =
    rest.span(pred) match {
      case (Nil, _ :: rest) => loop(acс, rest)
      case (res, _ :: rest) => loop(acс :+ res, rest)
      case (res, Nil) => acс :+ res
    }
  loop(Vector(), iterable)
}

def readFileLines(src: String): Try[Seq[String]] = 
  Using(Source.fromFile(src))(_.getLines().toSeq)