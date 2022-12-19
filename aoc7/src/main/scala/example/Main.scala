package example

import scala.io.Source
import scala.collection.mutable.ListBuffer

object Aoc7 extends App {
  private val file  = Source.fromResource("input")
  private val lines = file.getLines.toList

  private val root = DirEntry("", None)

  private val cmds = lines.foldLeft(root) {
    case (dir, ChangeDirCommand(cmd)) => cmd.getDir(dir)
    case (dir, "$ ls")                => dir
    case (dir, s"dir $name")          => dir += DirEntry(name, Some(dir))
    case (dir, s"$size $name")        => dir += FileEntry(name, size.toInt)
    case (dir, _)                     => dir
  }

  // Task 1
  println(root.flattenDirs.filter(_.getSize <= 100000).map(_.getSize).sum)

  // Task 2
  private val fsSize   = 70000000
  private val usage    = root.getSize
  private val required = 30000000
  private val toFree   = usage - fsSize + required
  println(root.flattenDirs.map(_.getSize).filter(_ >= toFree).sorted.head)
}

final case class ChangeDirCommand(private val dir: String) {

  def getDir(root: DirEntry): DirEntry = (root.parent, dir) match {
    case (Some(parent), "..") => parent
    case (Some(parent), "/")  => getDir(parent)
    case (None, "/")          => root
    case (_, subdir) =>
      root.entries
        .filter(_.isInstanceOf[DirEntry])
        .map(_.asInstanceOf[DirEntry])
        .find(_.name == subdir)
        .get
  }
}

object ChangeDirCommand {

  def unapply(cmd: String): Option[ChangeDirCommand] = cmd match {
    case s"$$ cd $dir" => Some(new ChangeDirCommand(dir))
    case _             => None
  }
}

sealed trait FsEntry {
  val name: String
  def getSize: Int
}

final case class FileEntry(name: String, _size: Int) extends FsEntry {
  override def getSize = _size
}

final case class DirEntry(name: String, parent: Option[DirEntry]) extends FsEntry {
  val entries               = ListBuffer[FsEntry]()
  override def getSize: Int = entries.map(_.getSize).sum

  def +=(entry: FsEntry) = {
    entries += entry
    this
  }

  def flattenDirs: List[DirEntry] =
    entries.filter(_.isInstanceOf[DirEntry]).map(_.asInstanceOf[DirEntry]).flatMap(x => x +: x.flattenDirs).toList

}
