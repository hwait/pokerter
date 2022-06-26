package cards.domain

import zio._

trait Deck {
  def shuffle(capacity: Int): Task[Unit]
  def oneCard(): Task[Card]
}
object Deck {
  // implementation
  case class CardsDeck(random: Random, queue: Queue[Card]) extends Deck {
    override def shuffle(capacity: Int): Task[Unit] =
      for {
        _ <- queue.takeAll // NOTE: not best, probably change via ref?
        seq <- random.shuffle(Chunk.range(0, capacity - 1))
        _ <- queue.offerAll(seq.map(i => Card(i)))
      } yield ()

    override def oneCard(): Task[Card] = 
      for {
        card <- queue.take
      } yield card
  }

  // layer
  val live: ZLayer[Random, Throwable, Deck] = ZLayer {
    for {
      random <- ZIO.service[Random]
      queue <- Queue.unbounded[Card]
    } yield CardsDeck(random, queue)
  }

  // accessor
  def shuffle(capacity: Int): ZIO[Deck, Throwable, Unit] =
    ZIO.serviceWithZIO(_.shuffle(capacity))
    
  def oneCard(): ZIO[Deck, Throwable, Card] =
    ZIO.serviceWithZIO(_.oneCard())
}
