package scalaGit.types

import java.util.Date
import java.nio.charset.StandardCharsets

case class User(name: String, email: String, ts: Date) {
  def fromUser(bytes: Seq[Byte]): Option[User] = {
    val name = new String(
      bytes.takeWhile(_ != '<').toArray,
      StandardCharsets.UTF_8
    ).trim()

    val info = new String(
      bytes.dropWhile(_ != '<').toArray,
      StandardCharsets.UTF_8
    ).trim()

    val infoIter = info.split(" ", 3).toIterator

    val email = infoIter.next().replaceAll("<", "").replaceAll(">", "")

    val time = new Date

    Option(User(name, email, time))
  }
}

