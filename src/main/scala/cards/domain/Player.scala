package cards.domain

import zio._

case class Player(n: Int, name: String, positionRef: Ref[Int], cardsRef: Ref[Chunk[Card]]) {
  def addCard(card: Task[Card]): ZIO[Random with Console, Throwable, Unit] = {
    for {
      c <- card
      _ <- cardsRef.update(_ :+ c)
    } yield ()
  }  
  def renew(maxIndex: Int): ZIO[Random with Console, Throwable, Unit] = {
    for {
      _ <- cardsRef.set(Chunk.empty[Card])
      sp <- positionRef.get
      _ <- positionRef.set (if (sp >= maxIndex) 0 else sp + 1)
    } yield ()
  }  
  def status(): ZIO[Console, Throwable, Unit] = {
    def positionText(pos: Int) = pos match
      case 0 => " on SB"
      case 1 => " on BB"
      case 2 => " on BTN"
      case _ => ""
    for {
      position <- positionRef.get
      console <- ZIO.service[Console]
      cards <- cardsRef.get
      _ <- console.printLine(s"$n: $name${positionText(position)} ${cards.mkString("[",",","]")}")
    } yield ()
  }
}

object Player {
  def init(n: Int): ZIO[Random with Console, Throwable, Player] = {
    for {
      positionRef <- Ref.make(n)
      cardsRef <- Ref.make(Chunk.empty[Card])
    } yield Player(n, s"player$n", positionRef, cardsRef)
  }  
}
