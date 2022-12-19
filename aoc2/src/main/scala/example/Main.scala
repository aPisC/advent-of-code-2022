package example

import java.util.IllegalFormatException
import scala.io.Source
import Shape._

sealed trait Shape

object Shape {
  object Rock extends Shape
  object Scissors extends Shape
  object Paper extends Shape

  def apply(char: String): Shape = char match {
    case "A" | "X" => (Rock)
    case "B" | "Y" => (Paper)
    case "C" | "Z" => (Scissors)
  }

  def unapply(char: String): Option[Shape] = char match {
    case "A" | "X" => Some(Rock)
    case "B" | "Y" => Some(Paper)
    case "C" | "Z" => Some(Scissors)
    case _         => None
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

case class Round(opponent: Shape, me: Shape) {
  def score = {
    val baseScore = me match {
      case Rock     => 1
      case Paper    => 2
      case Scissors => 3
    }

    val outcomeScore = (opponent, me) match {
      case (op, me) if me == Shape.stronger(op) => 6
      case (op, me) if me == op                 => 3
      case _                                    => 0
    }

    baseScore + outcomeScore
  }
}

object Round {
  // Parsing logic for task 1
  def parse1(line: String) = line.split(' ').filter(_.nonEmpty) match {
    case Array(Shape(a), Shape(b)) => Round(a, b)
    case _                         => throw new Exception()
  }

  def parse(task: Int)(line: String) = {
    task match {
      case 1 => parse1(line)
      case _ => parse2(line)
    }
  }

  // Parsing logic for task 2
  def parse2(line: String) = line match {
    case s"$a X" =>
      Round(Shape(a), Shape.weaker(Shape(a)))
    case s"$a Y" =>
      Round(Shape(a), Shape(a))
    case s"$a Z" =>
      Round(Shape(a), Shape.stronger(Shape(a)))
  }

  def test(round: Round) = {
    import round.{me => sanyi}
    val me = sanyi
  }
}

object Main extends App {

  private val filePath = "input/input";
  private val file = Source.fromFile(filePath)

  private val score = file.getLines
    .map(Round.parse(1))
    .map(_.score)
    .sum

  print(score)
  file.close
}
