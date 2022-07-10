package cards.logic

import zio._
import cards.domain.Card

/** Provides deck service for each game
 * 
 *  @param random service layer provides random generator
 *  @param queue all cards in the deck shuffled
 *  @param capacity max cards in the deck
 */
case class Deck(random: Random, queue: Queue[Card], capacity: Int) {
  
  /** Shuffles the whole deck */
  val shuffle: Task[Unit] =
    for {
      _ <- queue.takeAll // NOTE: not best, probably change via ref?
      seq <- random.shuffle(Chunk.range(0, capacity - 1))
      // seq = Chunk.range(0, capacity - 1)
      _ <- queue.offerAll(seq.map(i => Card(i)))
    } yield ()

  /** Provides one card from the deck */
  val oneCard: UIO[Card] = 
    for {
      card <- queue.take
    } yield card
}

/** Factory for [[cards.logic.Deck]] instances. */
object Deck {

  /** Creates a new deck
   *
   *  @param capacity max cards in the deck
   */
  def apply(capacity: Int): URIO[Random, Deck] =
    for {
      random <- ZIO.service[Random]
      queue <- Queue.unbounded[Card]
    } yield Deck(random, queue, capacity)
    
}
