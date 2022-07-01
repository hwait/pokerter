package cards.domain

import zio._
import java.util.UUID
import java.io.IOException

type PlayerId = UUID

case class Player( pid: PlayerId, n: Int, name: String, positionRef: Ref[Int], cardsRef: Ref[Chunk[Card]]) {
  def addCard(card: Task[Card]): Task[Unit] = {
    for {
      c <- card
      _ <- cardsRef.update(_ :+ c)
    } yield ()
  }
  
  def renew(maxIndex: Int): Task[Unit] = {
    for {
      _ <- cardsRef.set(Chunk.empty[Card])
      sp <- positionRef.get
      _ <- positionRef.set (if (sp >= maxIndex) 0 else sp + 1)
    } yield ()
  }

  // def move(promise: Promise[Nothing, Boolean]) = {
  def move(): ZIO[Dealer & Console, Throwable, Unit] = {
    for {
      dealer <- ZIO.service[Dealer]
      _ <- dealer.status()
    } yield ()
  }

  def status(): ZIO[Console, IOException, Unit] = {
    def positionText(pos: Int) = pos match
      case 0 => " on SB"
      case 1 => " on BB"
      case 2 => " on BTN"
      case _ => ""

    for {
      position <- positionRef.get
      cards <- cardsRef.get
      console <- ZIO.service[Console]
      _ <- console.printLine(s"$n: $name${positionText(position)} ${cards.mkString("[",",","]")}")
      // _ <- ZIO.attempt(println(s"$n: $name${positionText(position)} ${cards.mkString("[",",","]")}"))
    } yield ()
  }
}

object Player {
  def init(pid: UUID, n: Int): Task[Player] = {
    for {
      positionRef <- Ref.make(n)
      cardsRef <- Ref.make(Chunk.empty[Card])
    } yield Player(pid, n, s"player$n", positionRef, cardsRef)
  }  
}
