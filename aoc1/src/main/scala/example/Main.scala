package example

import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

object Main extends App {

  private val inputFilePath = "input/input"
  private val file = Source.fromFile(inputFilePath)
  private val lines = file.getLines()

  private def recursionOuter(inputs: Iterator[String]) = {
    
    def recursionInner(inputs: Iterator[String]) : Array[Int] = {
      val line = inputs.next()

      line match {
        case "" => ??
        case line => ?? :+ line
      }

    }

    return elem :: recursion()
  }
}

// object Main extends App {

//   private val inputFilePath = "input/input"
//   private val file = Source.fromFile(inputFilePath)
//   private val lines = file.getLines()

//   private val (smurfsBuilder, _) = lines
//     .map(_.toIntOption)
//     .foldLeft((Array.newBuilder[Array[Int]], Array.newBuilder[Int])) {
//       case ((smurfs, builder), None) =>
//         (smurfs += builder.result(), Array.newBuilder[Int])
//       case ((smurfs, builder), Some(line)) => (smurfs, builder += line.toInt)
//     }

//   file.close()
//   private val smurfs = smurfsBuilder.result()

//   private val sums = smurfs.map(_.sum)
//   private val max = sums.max
//   private val top3 = sums.sorted(Ordering.Int.reverse).take(3).sum

//   print(max, top3)
// }
