package cards.logic

import zio._
import cards.domain.Card

case class Deck(random: Random, queue: Queue[Card], capacity: Int) {
  val shuffle: Task[Unit] =
    for {
      _ <- queue.takeAll // NOTE: not best, probably change via ref?
      seq <- random.shuffle(Chunk.range(0, capacity - 1))
      // seq = Chunk.range(0, capacity - 1)
      _ <- queue.offerAll(seq.map(i => Card(i)))
    } yield ()

  val oneCard: Task[Card] = 
    for {
      card <- queue.take
    } yield card
}

object Deck {
  def apply(capacity: Int): ZIO[Random, Nothing, Deck] =
    for {
      random <- ZIO.service[Random]
      queue <- Queue.unbounded[Card]
    } yield Deck(random, queue, capacity)
    
}