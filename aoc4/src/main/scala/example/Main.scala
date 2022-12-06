package example

import scala.io.Source
import java.util.Random
import java.util.IllegalFormatException

case class Range(start: Int, end: Int) {
  def contains(other: Range) =
    start <= other.start && end >= other.end

  def overlaps(other: Range) =
    (this contains other) || (other contains this)

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
    .map {
      case s"$a-$b,$c-$d" =>
        Some((Range(a.toInt, b.toInt), Range(c.toInt, d.toInt)))
      case _ => None
    }
    .map {
      // case Some((r1, r2)) if r1 intersects r2  => 1
      case Some((r1, r2)) if r1 overlaps r2 => 1
      case _                                => 0
    }
    .sum

  file.close()
  println(result)
}
