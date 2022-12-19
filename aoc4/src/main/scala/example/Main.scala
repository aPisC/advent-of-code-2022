package example

import scala.language.implicitConversions
import scala.io.Source
import java.util.Random
import java.util.IllegalFormatException

object Main extends App {
  val path = "inputs/input"
  val file = Source.fromFile(path)

  val result = file.getLines
    .map {
      case s"$a-$b,$c-$d" =>
        Some((Range(a.toInt, b.toInt + 1), Range(c.toInt, d.toInt + 1)))
      case _ => None
    }
    .map {
      // case Some((r1, r2)) if r1 intersects r2 => 1
      case Some((r1, r2)) if r1.overlaps(r2) => 1
      case _                                 => 0
    }
    .sum

  file.close()
  println(result)
}
