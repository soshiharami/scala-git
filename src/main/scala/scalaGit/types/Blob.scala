package scalaGit.types

import java.nio.charset.StandardCharsets
import scala.util.Try

case class Blob(size: Int, content: String) {
  def newBlob(content: String): Blob = {
    Blob(content.length, content)
  }

  def fromBlob(bytes: Seq[Byte]): Try[Blob] = {
    Try {
      val content = new String(bytes.toArray, StandardCharsets.UTF_8)
      Blob(content.length, content)
    }
  }

  def asBytes(blob: Blob): Seq[Byte] = {
    val header = s"blob ${blob.size} \u0000"
    val store = s"${header} ${blob.toString}"

    store.getBytes()
  }

  def sha1(s: String) = java.security.MessageDigest
    .getInstance("SHA-1")
    .digest(s.getBytes)
    .map((b: Byte) =>
      (if (b >= 0 & b < 16) "0" else "") + (b & 0xff).toHexString
    )
    .mkString
    .getBytes()

  def calcHash(blob: Blob): Seq[Byte] = {
    sha1(Blob.toString())
  }
}
