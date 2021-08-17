package scalaGit.func

import java.nio.charset.StandardCharsets

object splitBytes {
  implicit class splitBytes(val bytes: Seq[Byte]) {
    def splitBytes(
        str: String,
        num: Int = 0
    ): Iterator[Array[Byte]] =
      new String(bytes.toArray, StandardCharsets.UTF_8)
        .split(str, 0)
        .map { case string => string.getBytes(StandardCharsets.UTF_8) }
        .iterator

    def mkString = new String(bytes.toArray, StandardCharsets.UTF_8)
  }
}
