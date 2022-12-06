package example

import scala.io.Source

object Main2 extends App {
  private val filepath = "inputs/input"
  private val file = Source.fromFile(filepath)

  private val result = file.getLines
    .foldLeft[
      (
          List[(String, String, String)],
          (Option[String], Option[String])
      )
    ]((Nil, (None, None))) {
      case ((result, (None, None)), line) =>
        (result, (Some(line), None))
      case ((result, (v1, None)), line) =>
        (result, (v1, Some(line)))
      case ((result, (Some(v1), Some(v2))), line) =>
        (result :+ (v1, v2, line), (None, None))
      case (r, _) => r
    }
    ._1
    .map((t) => t._1.find(c => t._2.contains(c) && t._3.contains(c)))
    .map(_.map(_.toInt))
    .map {
      case Some(i) if 65 until 91 contains i  => i - 65 + 27
      case Some(i) if 97 until 123 contains i => i - 97 + 1
      case _                                  => 0
    }
    .sum

  file.close()
  println(result)
}

object Main1 extends App {
  private val filepath = "inputs/input"
  private val file = Source.fromFile(filepath)

  private val result = file.getLines
    .map(l => (l.substring(0, l.length / 2), l.substring(l.length / 2)))
    .map((a) => a._1.find(c => a._2.contains(c)))
    .map(_.map(_.toInt))
    .map {
      case Some(i) if 65 until 91 contains i  => i - 65 + 27
      case Some(i) if 97 until 123 contains i => i - 97 + 1
      case _                                  => 0
    }
    .sum

  file.close()
  println(result)
}
