package cards.logic

import zio._
import java.io.IOException
import cards.domain._
import cards.services.Logger

case class Player(
    pid: PlayerId,
    n: Int,
    name: String,
    position: Int,
    statusRef: Ref[PlayerStatus],
    bankRef: Ref[Int],
    investedRef: Ref[Int],
    holeCardsRef: Ref[Chunk[Card]],
    console: Console 
  ) {

  def addCard(card: Task[Card]): Task[Unit] = {
    for {
      c <- card
      _ <- holeCardsRef.update(_ :+ c)
    } yield ()
  }
  
  val renew: Task[Unit] = {
    for {
      _ <- statusRef.set(PlayerStatus.Active)
      _ <- investedRef.set(0)
      _ <- holeCardsRef.set(Chunk.empty[Card])
    } yield ()
  }

  def bet(biggestWager: Int, wager: Int, action: PlayerActions, stage: GameStages) = 
    for {
      clock <- ZIO.service[Clock]
      dt <- clock.nanoTime
      bank <- bankRef.get
      invested <- investedRef.get 
      // _ <- console.printLine(s"$name on $position with ($bank): $action $wager against $biggestWager, $invested already invested ---> ")
      toClose = if (bank >= wager - invested) wager - invested else bank
      _ <- bankRef.set(bank - toClose)
      _ <- investedRef.set(invested + toClose)
      // _ <- console.printLine(s"$toClose wagered, so total invested ${invested + toClose}, new bank (${bank - toClose})")
      
    } yield PlayerAction(pid, name, position, stage, action, toClose, dt)

  def noBet(action: PlayerActions, stage: GameStages) = 
    for {
      clock <- ZIO.service[Clock]
      dt <- clock.nanoTime
      bank <- bankRef.get
      invested <- investedRef.get 
      // _ <- console.printLine(s"$name on $position with ($bank): $action, $invested invested")
    } yield PlayerAction(pid, name, position, stage, action, 0, dt)
  
  def move(gameStageN: Int, actions: Chunk[PlayerAction], biggestWager: Int, blindes: Blindes) =
    for {
      gameStage <- ZIO.succeed(GameStages.fromOrdinal(gameStageN))
      pa <- gameStage  match {
        case GameStages.Preflop => preflop(gameStage, actions, biggestWager, blindes)
        case GameStages.Flop  => flop(gameStage, actions, biggestWager)
        case GameStages.Turn  => turn(gameStage, actions, biggestWager)
        case GameStages.River  => river(gameStage, actions, biggestWager)
      }
    } yield pa

  def preflop(gameStage: GameStages, actions: Chunk[PlayerAction], biggestWager: Int, blindes: Blindes) = {
    for {
      invested <- investedRef.get 
      pa <- (biggestWager, invested, position) match
        case (0, 0, 0) => 
          bet(0, blindes.sb, PlayerActions.Bet, gameStage)
        case (blindes.sb, 0, 1) => 
          bet(blindes.sb, blindes.bb, PlayerActions.Bet, gameStage)
        case _ => 
            bet(biggestWager, biggestWager, PlayerActions.Call, gameStage) 
    } yield pa
  }

  def flop(gameStage: GameStages, actions: Chunk[PlayerAction], biggestWager: Int) = {
    for {
      // _ <- console.printLine(s"[FLOP] $name on $position ")
      pa <- noBet(PlayerActions.Check, gameStage)
    } yield pa
  }


  def turn(gameStage: GameStages, actions: Chunk[PlayerAction], biggestWager: Int) = {
    for {
      // _ <- console.printLine(s"[TURN] $name on $position ")
      pa <- noBet(PlayerActions.Check, gameStage)
    } yield pa
  }


  def river(gameStage: GameStages, actions: Chunk[PlayerAction], biggestWager: Int) = {
    for {
      // _ <- console.printLine(s"[RIVER] $name on $position ")
      pa <- noBet(PlayerActions.Check, gameStage)
    } yield pa
  }

  val status: ZIO[Console, IOException, Unit] = {
    def positionText(pos: Int) = pos match
      case 0 => " on SB"
      case 1 => " on BB"
      // case 2 => " on BTN"
      case _ => s" on $pos "

    for {
      cards <- holeCardsRef.get
      _ <- console.printLine(s"$n: $name${positionText(position)} ${cards.mkString("[",",","]")}")
    } yield ()
  }
}

object Player {
  def apply(pid: PlayerId, n: Int, position: Int, status: PlayerStatus, bank: Int): ZIO[Console, Nothing, Player] = {
    for {
      statusRef <- Ref.make(status)
      bankRef <- Ref.make(bank)
      investedRef <- Ref.make(0)
      holeCardsRef <- Ref.make(Chunk.empty[Card])
      console <- ZIO.service[Console]
    } yield Player(pid, n, s"player$n", position, statusRef, bankRef, investedRef,  holeCardsRef, console)
  }  
}
