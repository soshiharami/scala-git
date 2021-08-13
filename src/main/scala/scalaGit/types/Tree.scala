package scalaGit.types

import java.nio.charset.StandardCharsets
import scala.util.Try
import java.nio.ByteBuffer

case class File(mode: Int, name: String, hash: Seq[Byte]) {
  def fromFile(header: Seq[Byte], hash: Seq[Byte]): Try[File] = {
    Try {
      val splitHeader = new String(header.toArray, StandardCharsets.UTF_8)

      val iter = splitHeader.split("\\s").filter(_.nonEmpty).toIterator

      val mode = iter.next.toInt

      val name = iter.next()

      File(mode, name, hash)
    }
  }

  case class Tree(contents: Seq[File]) {
    def fromTree(bytes: Seq[Byte]): Try[Tree] = {
      Try {
        val contents: Seq[File] = Seq()
        val iter = bytes.split('\u0000').invert.toIterator

        val header = iter.next()
        val content = iter.foldLeft()
      }
    }
  }

  def splitByte(bytes: Seq[Byte]) = {
    ByteBuffer.wrap(bytes)
  }
}
