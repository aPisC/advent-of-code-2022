package example

import scala.io.Source

object Shapes extends Enumeration {
  type Shape = Value

  val Rock, Scissors, Paper: Shape = Value

  def parse(char: Char): Shape = {
    char match {
      case 'A' | 'X' => (Rock)
      case 'B' | 'Y' => (Paper)
      case 'C' | 'Z' => (Scissors)
    }
  }

  def stronger(shape: Shape) = {
    shape match {
      case Rock     => Paper
      case Paper    => Scissors
      case Scissors => Rock
    }
  }

  def weaker(shape: Shape) = {
    shape match {
      case Paper    => Rock
      case Scissors => Paper
      case Rock     => Scissors
    }
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
  def parse(line: String) = {
    Round(Shapes.parse(line.charAt(0)), Shapes.parse(line.charAt(2)))
  }

  def parse2(line: String) = {
    val op = Shapes.parse(line.charAt(0))
    val me = line.charAt(2) match {
      case 'X' => Shapes.weaker(op)
      case 'Z' => Shapes.stronger(op)
      case _   => op
    }

    Round(op, me)
  }
}

object Main extends App {

  private val filePath = "input/input";
  private val file = Source.fromFile(filePath)
  private val rounds = file.getLines.map(Round parse _)

  private val score = rounds.map(_.score).sum

  print(score)
  file.close

}
