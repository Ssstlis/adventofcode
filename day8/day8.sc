import $file.util
import util._

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Try

final case class Node[T](rt: T, leafs: Set[Node[T]])

val goldKey = "shiny gold"
val specLines: Try[Seq[String]] = readFileLines("spec.txt")
val testLines: Try[Seq[String]] = readFileLines("data.txt")

def prepare(seq: Seq[String]) = {
  val splitted: Map[String, List[String]] = seq
    .map(_.split(" bags contain ").toList)
    .map(a => (a(0), a(1).split(", ").map(_.replaceAll("\\.", "")).toList))
    .toMap
  val bagsChange: String => String = _.dropWhile(_ != ' ').dropRight(4).trim
  splitted.view.mapValues {
    case "no other bags" :: Nil => Nil
    case lst => lst.map(bagsChange)
  }.toMap - goldKey
}

def prepare1(seq: Seq[String]) = {
  val splitted: Map[String, List[String]] = seq
    .map(_.split(" bags contain ").toList)
    .map(a => (a(0), a(1).split(", ").map(_.replaceAll("\\.", "")).toList))
    .toMap

  val spanned = (_: String).span(_ != ' ') match {
    case (a, b) => (a.toInt, b.trim)
  }

  val bagsChange: String => (Int, String) = s => spanned(s.dropRight(4).trim)
  splitted.view.mapValues {
    case "no other bags" :: Nil => Nil
    case lst => lst.map(bagsChange)
  }.toMap
}

def count(map: Map[String, List[String]]): Int = {

  @tailrec
  def loop(rest: Map[String, List[String]], queue: Queue[String], acc: Int): Int = {
    if (queue.isEmpty) acc else {
      queue.dequeue match {
        case (head, restQ) =>
          val (filtered, newMap) = rest.partition { case (_, leafs) => leafs.contains(head) }
          loop(newMap, restQ.appendedAll(filtered.keys), acc + filtered.size)
      }
    }
  }
  loop(map, Queue(goldKey), 0)
}

def f = prepare _ andThen count



def count1(map: Map[String, List[(Int, String)]]): Int = {
  @tailrec
  def loop(rest: Map[String, List[(Int, String)]], queue: Queue[(Int, String)], acc: Int): Int = {
    if (queue.isEmpty) acc else {
      queue.dequeue match {
        case ((m, head), restQ) =>
          rest.get(head) match {
            case Some(value) =>
              loop(rest, restQ.appendedAll(value.map(t => (t._1 * m, t._2))), acc + m)
            case None => loop(rest, restQ, acc + m)
          }
      }
    }
  }
  loop(map, Queue((1, goldKey)), 0) - 1
}

def f1 = prepare1 _ andThen count1

assert(specLines.map(f) == Try(4))
println(testLines.map(f))
println(())
assert(specLines.map(f1) == Try(32))
println(testLines.map(f1))


//((1, dark olive), (2, vibrant plum)), 0
//((2, vibrant plum), (3, faded blue), (4, dotted black)), 1
//((3, faded blue), (4, dotted black), (10, faded blue), (12, dotted black)), 3
//((4, dotted black), (10, faded blue), (12, dotted black)), 6
//((10, faded blue), (12, dotted black)), 10
//((12, dotted black)), 20
//(), 32