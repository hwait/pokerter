package cards.domain

import zio._

case class Deck(random: Random, queue: Queue[Card]) {
  def shuffle(capacity: Int): Task[Unit] =
    for {
      _ <- queue.takeAll // NOTE: not best, probably change via ref?
      seq <- random.shuffle(Chunk.range(0, capacity - 1))
      _ <- queue.offerAll(seq.map(i => Card(i)))
    } yield ()

  def oneCard(): Task[Card] = 
    for {
      card <- queue.take
    } yield card
}

object Deck {
  def apply(capacity: Int): ZIO[Random, Nothing, Deck] =
    for {
      random <- ZIO.service[Random]
      queue <- Queue.unbounded[Card]
    } yield Deck(random, queue)
    
}
