package example

import scala.io.Source

object Shapes extends Enumeration {
  type Shape = Value

  val Rock, Scissors, Paper: Shape = Value

  def parse(char: String): Shape = char match {
    case "A" | "X" => (Rock)
    case "B" | "Y" => (Paper)
    case "C" | "Z" => (Scissors)
  }

  def stronger(shape: Shape) = shape match {
    case Rock     => Paper
    case Paper    => Scissors
    case Scissors => Rock
  }

  def weaker(shape: Shape) = shape match {
    case Paper    => Rock
    case Scissors => Paper
    case Rock     => Scissors
  }
}

case class Round(opponent: Shapes.Shape, me: Shapes.Shape) {
  def score = {
    val baseScore = me match {
      case Shapes.Rock     => 1
      case Shapes.Paper    => 2
      case Shapes.Scissors => 3
    }

    val outcomeScore = (opponent, me) match {
      case (op, me) if me == Shapes.stronger(op) => 6
      case (op, me) if me == op                  => 3
      case _                                     => 0
    }

    baseScore + outcomeScore
  }
}

object Round {
  // Parsing logic for task 1
  def parse1(line: String) = line match {
    case s"$a $b" => Round(Shapes.parse(a), Shapes.parse(b))
  }

  // Parsing logic for task 2
  def parse2(line: String) = line match {
    case s"$a X" =>
      Round(Shapes.parse(a), Shapes.weaker(Shapes.parse(a)))
    case s"$a Y" =>
      Round(Shapes.parse(a), Shapes.parse(a))
    case s"$a Z" =>
      Round(Shapes.parse(a), Shapes.stronger(Shapes.parse(a)))
  }
}

object Main extends App {

  private val filePath = "input/input";
  private val file = Source.fromFile(filePath)

  private val score = file.getLines
    .map(Round parse1 _)
    .map(_.score)
    .sum

  print(score)
  file.close

}
