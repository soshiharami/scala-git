package scalaGit.types

import scalaGit.func.splitBytes.splitBytes
import java.nio.charset.StandardCharsets
import scala.util.Try
import java.nio.ByteBuffer

case class File(mode: Int, name: String, hash: Seq[Byte]) {
  def fromFile(header: Seq[Byte], hash: Seq[Byte]): Try[File] = {
    Try {
      val splitHeader = new String(header.toArray, StandardCharsets.UTF_8)

      val iter = splitHeader.split("\\s").filter(_.nonEmpty).iterator

      val mode = iter.next.toInt

      val name = iter.next()

      File(mode, name, hash)
    }
  }

  def encode(file: File): Seq[Byte] = {
    val header = s"${file.mode} ${file.name}\u0000"
    header.getBytes(StandardCharsets.UTF_8) :++ file.hash
  }
}

case class Tree(contents: Seq[File]) {
  def fromTree(bytes: Seq[Byte]): Try[Tree] = {
    Try {
      var contents: Seq[File] = Seq()
      val iter = bytes.splitBytes("\u0000")
      var header = iter.next()
      contents = iter.foldLeft(contents) { (acc, x) =>
        {
          val (hash, next_header) = x.splitAt(20)
          val file = File(0, "", Seq()).fromFile(header, hash).get

          acc.:+(file)
          header = next_header
          acc
        }
      }
      Tree(contents)
    }
  }

  def asByte(tree: Tree): Seq[Byte] = {
    val content: Seq[Byte] =
      contents.iterator.flatMap(x => x.encode(x)).toSeq
    val header = s"tree ${content.length}\u0000"

    header.getBytes(StandardCharsets.UTF_8) :++ content
  }
}
