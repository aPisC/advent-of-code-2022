package example

import scala.io.Source
import scala.collection.mutable.Builder

object Main extends App {
  val path = "inputs/input"
  val file = Source.fromFile(path)

  val columnCount = 9

  val lines   = file.getLines()
  val columns = parseColumns(lines, List.fill(columnCount)(Nil))

  // val arrangedColumns = lines.foldLeft(columns) {
  //   case (columns, s"move $count from $source to $target") =>
  //     ()
  // }

  print(0)

  def arrangeColumns(
    columns: List[List[String]],
    from: Int,
    to: Int,
    count: Int
  ) = {
    columns.zipWithIndex.map {
      case (col, i) if i == from => col.drop(count)
      case (col, i) if i == to =>
        columns(from).take(count).reverse :: col
      case (col, i) => col
    }
  }

  def parseColumns(
    lines: Iterator[String],
    columns: List[List[String]]
  ): List[List[String]] = {
    if (!lines.hasNext) columns
    else
      lines.next match {
        case "" => columns
        case line =>
          parseColumns(lines, columns)
            .zip(parseBoxesLine(line, columns.length))
            .map {
              case (column, None)    => column
              case (column, Some(c)) => c +: column
            }
            .toList
      }
  }

  def parseBoxesLine(
    line: String,
    count: Int,
    boxes: List[Option[String]] = Nil
  ): List[Option[String]] =
    (line, count) match {
      case (_, 0) => boxes
      case (s"[$box] $rest", i) =>
        parseBoxesLine(rest, i - 1, boxes :+ Some(box))
      case (s"[$box]", i)    => parseBoxesLine("", i - 1, boxes :+ Some(box))
      case (s"    $rest", i) => parseBoxesLine(rest, i - 1, boxes :+ None)
      case (_, i)            => parseBoxesLine("", i - 1, boxes :+ None)
    }

}
