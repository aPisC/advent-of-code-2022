package example

import scala.io.Source

object Hello extends App {
  private val file = Source.fromResource("input")

  private val (_, cycles) = file.getLines.foldLeft((1, List[Int]())) {
    case ((reg,  values), s"addx $inc") => (reg + inc.toInt, values :+ (reg) :+ (reg))
    case ((reg,  values), _) => (reg ,  values :+ (reg))
  }

  private val result1 = cycles
    .drop(19)
    .grouped(40)
    .map(_.head)
    .zipWithIndex
    .map {case (v, i) => v*(i*40 + 20)}
    .sum
  println(result1)


  cycles
    .grouped(40)
    .map(row => row.zipWithIndex.map {case (c, i) => if (i >= c-1 && i <= c+1) "#" else " "}.mkString(""))
    .foreach(println)
}

