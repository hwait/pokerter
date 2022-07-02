package cards.domain

import zio._
import java.util.UUID
import java.io.IOException

type PlayerId = UUID

case class Player(console: Console, pid: PlayerId, n: Int, table: Table, name: String, position: Int, cardsRef: Ref[Chunk[Card]]) {
  def addCard(card: Task[Card]): Task[Unit] = {
    for {
      c <- card
      _ <- cardsRef.update(_ :+ c)
    } yield ()
  }
  
  def renew(maxIndex: Int): Task[Unit] = {
    for {
      _ <- cardsRef.set(Chunk.empty[Card])
    } yield ()
  }

  // def move(promise: Promise[Nothing, Boolean]) = {
  def move(): ZIO[Console, Throwable, Unit] = {
    for {
      _ <- table.status()
    } yield ()
  }

  def status(): ZIO[Console, IOException, Unit] = {
    def positionText(pos: Int) = pos match
      case 0 => " on SB"
      case 1 => " on BB"
      case 2 => " on BTN"
      case _ => ""

    for {
      cards <- cardsRef.get
      _ <- console.printLine(s"$n: $name${positionText(position)} ${cards.mkString("[",",","]")}")
      // _ <- ZIO.attempt(println(s"$n: $name${positionText(position)} ${cards.mkString("[",",","]")}"))
    } yield ()
  }
}

object Player {
  def apply(pid: UUID, n: Int, position: Int, table: Table): ZIO[Console, Nothing, Player] = {
    for {
      cardsRef <- Ref.make(Chunk.empty[Card])
      console <- ZIO.service[Console]
    } yield Player(console, pid, n, table, s"player$n", position, cardsRef)
  }  
}
