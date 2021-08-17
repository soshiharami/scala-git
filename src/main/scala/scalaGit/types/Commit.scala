package scalaGit.types

import scalaGit.func.splitBytes.splitBytes
import java.util.Date
import java.nio.charset.StandardCharsets
import scala.util.{Success, Failure}

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

    val infoIter = info.split(" ", 3).iterator

    val email = infoIter.next().replaceAll("<", "").replaceAll(">", "")

    val time = new Date

    Option(User(name, email, time))
  }
}

case class Commit(
    tree: String,
    parent: Option[String],
    author: User,
    commiter: User,
    message: String
) {
  def fromCommit(bytes: Seq[Byte]): Option[Commit] = {
    val iter: Iterator[Array[Byte]] = bytes.splitBytes("\n")

    val tree: String = iter
      .next()
      .toSeq
      .splitBytes(" ", 2)
      .drop(1)
      .flatMap(x => x)
      .toArray
      .mkString

    val parent = iter
      .next()
      .toSeq
      .splitBytes(" ", 2)
      .toArray
      .map(_.mkString)
      .andThen(x =>
        x.apply(0) match {
          case "parent" => Right(x.apply(1))
          case _ =>
            Left(Array(x.aplly(0).getBytes, " ", x.aplly(1).getBytes))
        }
      )

    val author = (parent match {
      case Right(_) => iter.next().toArray()
      case Left(s)  => s
    })

  }
}
