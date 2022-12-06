package example

import scala.io.Source
import java.util.Random

case class Range(start: Int, end: Int) {
  def contains(other: Range) = start <= other.start && end >= other.end
  def intersects(other: Range) =
    contains(other) ||
      other.contains(this) ||
      (start until (end + 1) contains other.start) ||
      (start until (end + 1) contains other.end)

}

object Main extends App {
  val path = "inputs/input"
  val file = Source.fromFile(path)
  val result = file.getLines
    .map(l => {
      val sl = l.split(',')
      val r1 = Range(sl(0).split('-')(0).toInt, sl(0).split('-')(1).toInt)
      val r2 = Range(sl(1).split('-')(0).toInt, sl(1).split('-')(1).toInt)
      (r1, r2)
    })

    // // For task 1
    // .map {
    //   case (r1, r2) if r1.contains(r2) => 1
    //   case (r1, r2) if r2.contains(r1) => 1
    //   case _                           => 0
    // }

    // For task 2
    .map {
      case (r1, r2) if r1.intersects(r2) => 1
      case _                             => 0
    }
    .sum

  println(result)
}
