/*
  - resources
  - dliub build
 */

package example

import scala.io.Source

object Main2 extends App {
  private val filepath = "inputs/input"
  private val file = Source.fromFile(filepath)

  private val result = file.getLines
    .grouped(3)
    .map { case Seq(a, b, c) =>
      a.find(char => (b contains char) && (c contains char))
    }
    .collect { case Some(char) => char.toInt }
    .map {
      case (i) if 65 until 91 contains i  => i - 65 + 27
      case (i) if 97 until 123 contains i => i - 97 + 1
      case _                              => 0
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
    .map { case (first, second) => first.find(second.contains(_)) }
    .collect { case Some(value) => value.toInt }
    .map {
      case (i) if 65 until 91 contains i  => i - 65 + 27
      case (i) if 97 until 123 contains i => i - 97 + 1
      case _                              => 0
    }
    .sum

  file.close()
  println(result)
}
