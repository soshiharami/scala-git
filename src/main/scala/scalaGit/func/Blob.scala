package scalaGit.func

import scalaGit.types.Blob

import java.nio.charset.StandardCharsets
import scala.util.Try

object BlobFunc {
  def newBlob(content: String): Blob = {
    Blob(content.length, content)
  }

  def fromBlob(bytes: Array[Byte]): Try[Blob] = {
    Try {
      val content = new String(bytes, StandardCharsets.UTF_8)
      Blob(content.length, content)
    }
  }

  def asBytes(blob: Blob): Array[Byte] = {
    val header = s"blob ${blob.size} \\0"
    val store = s"${header} ${blob.toString}"

    Array(fromBlob(store.getBytes()).get)
  }
}
