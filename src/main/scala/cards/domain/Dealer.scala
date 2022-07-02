package cards.domain

import zio._
import GameConfig._

trait Dealer {
  def init(game: GameConfig, playersNumber: Int, blindes: Blindes): Task[Unit]
  def playGame(): ZIO[Dealer & Console, Throwable, Unit]
  def status(): ZIO[Console, Throwable, Unit]
}

object Dealer {
  case class DealerImpl(
    random: Random, 
    console: Console, 
    deck: Deck, 
    playersRef: Ref[Chunk[Player]], 
    gameRef: Ref[GameConfig],
    blindesRef: Ref[Blindes]
  ) extends Dealer {

    def init(game: GameConfig, playersNumber: Int, blindes: Blindes): Task[Unit] = 
      for {
        pid <- random.nextUUID
        // players <- ZIO.foreach(Chunk.range(0, playersNumber))(n => Player.init(pid, n))
        // _ <- playersRef.set(players)
        _ <- gameRef.set(game)
        _ <- blindesRef.set(blindes)
      } yield ()

    def playGame(): ZIO[Dealer & Console, Throwable, Unit] = 
      for {
        game <- gameRef.get
        _ <- deck.shuffle(game.cardsInDeck)
        players <- playersRef.get
        _ <- ZIO.foreach(players)(p => p.renew(players.size-1))
        _ <- ZIO.foreach(players)(p => p.addCard(deck.oneCard())).repeatN(game.cardsInHand-1)
        _ <- players(0).move()
      } yield ()
  
    def status(): ZIO[Console, Throwable, Unit] = 
      for {
        game <- gameRef.get
        players <- playersRef.get
        blindes <- blindesRef.get
        _ <- console.printLine(s"[${game.limit} ${game.name} ${blindes.sb}/${blindes.bb} for ${players.size} players]: ")
        _ <-  ZIO.foreachDiscard(players)(p => p.status())
      } yield ()
  }

  val live: ZLayer[Deck & Console & Random, Nothing, Dealer] = ZLayer {
    for {
      deck <- ZIO.service[Deck]
      console <- ZIO.service[Console]
      random <- ZIO.service[Random]
      playersRef <- Ref.make(Chunk.empty[Player])
      gameRef <- Ref.make(GameConfig.NLHoldem)
      blindesRef <- Ref.make(Blindes(0,0,0))
    } yield DealerImpl(random, console, deck, playersRef, gameRef, blindesRef)
  }

  // accessors
  def init(game: GameConfig, playersNumber: Int, blindes: Blindes): ZIO[Dealer, Throwable, Unit] =
    ZIO.serviceWithZIO(_.init(game, playersNumber, blindes))

  def playGame(): ZIO[Dealer & Console, Throwable, Unit] =
    ZIO.serviceWithZIO[Dealer](_.playGame())

  def status(): ZIO[Dealer & Console, Throwable, Unit] =
    ZIO.serviceWithZIO[Dealer](_.status())
  
}