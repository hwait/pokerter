package cards.domain

import zio._
import java.util.UUID

enum PlayerActions(val name: String, val aggressive: Boolean):
  case Fold extends PlayerActions("Fold", false)
  case FoldDisconnected extends PlayerActions("FoldDisconnected", false)
  case Check extends PlayerActions("Check", false)
  case CheckDisconnected extends PlayerActions("CheckDisconnected", false)
  case Call extends PlayerActions("Call", false)
  case PostSB extends PlayerActions("PostSB", false)
  case PostBB extends PlayerActions("PostBB", false)
  case Bet extends PlayerActions("Bet", true)
  case Raise extends PlayerActions("Raise", true)
end PlayerActions

case class PokerCombinations(id: Int, val name: String, val description: String)

enum DealerActions(val name: String):
  case DealsFlop extends DealerActions("DealsFlop")
  case DealsTurn extends DealerActions("DealsTurn")
  case DealsRiver extends DealerActions("DealsRiver")
end DealerActions

enum GameStages(val n: Int):
  case Preflop extends GameStages(0)
  case Flop extends GameStages(1)
  case Turn extends GameStages(2)
  case River extends GameStages(3)
end GameStages

enum GameResults(val n: Int):
  case Lost extends GameResults(0)
  case LostSidePot extends GameResults(1)
  case Tied extends GameResults(2)
  case Won extends GameResults(3)
  case WonSidePot extends GameResults(4)
end GameResults

trait GameActions

case class PlayerAction(pid: PlayerId, position: Int, action: PlayerActions, stage: GameStages, wager: Int, dt: Long) extends GameActions {
  override def toString(): String = {
    val bet = if (wager>0) s" on $wager" else ""
    s"$stage: [$pid] on $position did $action$bet at $dt"
  }
}

case class DealerAction(action: DealerActions, cards: Chunk[Card], dt: Long) extends GameActions {
  override def toString(): String = {
    val cardsText = cards.mkString("[","","]")
    s"DEALER: deals $cardsText at $dt"
  }
}

case class PlayerEvaluation(pid: PlayerId, result: GameResults, topCombination: PokerCombinations, cards: Chunk[Card], profit: Int, dt: Long) extends GameActions {
  override def toString(): String = {
    val cardsText = cards.mkString("[","","]")
    s"[$pid] $result $profit with $topCombination $cardsText"
  }
}

trait Logger {
  def playerMoved(pid: PlayerId, position: Int, action: PlayerActions, stage: GameStages, wager: Int = 0): Task[Unit]
  def dealerMoved(action: DealerActions, cards: Chunk[Card]): Task[Unit]
  def playerEvaluated(pid: PlayerId, result: GameResults, topCombination: PokerCombinations, cards: Chunk[Card], profit: Int): Task[Unit]
  def saveAndClear(): Task[Unit]
  // TODO: Calc current pot from the logs, for check the Dealer's pot sustainability
}
object Logger {
  // implementation
  case class LoggerImpl(clock: Clock, console: Console, actionsRef: Ref[Chunk[GameActions]]) extends Logger {
    override def playerMoved(pid: PlayerId, position: Int, action: PlayerActions, stage: GameStages, wager: Int): Task[Unit] =
      for {
        currentTime <- clock.nanoTime
          _ <- actionsRef.update(_ :+ PlayerAction(pid, position, action, stage, wager, currentTime))
      } yield ()

    override def dealerMoved(action: DealerActions, cards: Chunk[Card]): Task[Unit] =
      for {
        currentTime <- clock.nanoTime
          _ <- actionsRef.update(_ :+ DealerAction(action, cards, currentTime))
      } yield ()

    override def playerEvaluated(pid: PlayerId, result: GameResults, topCombination: PokerCombinations, cards: Chunk[Card], profit: Int): Task[Unit] =
      for {
        currentTime <- clock.nanoTime
          _ <- actionsRef.update(_ :+ PlayerEvaluation(pid, result, topCombination, cards, profit, currentTime))
      } yield ()

    override def saveAndClear(): Task[Unit] = 
      for {
        /*
          Just print to console instead of save in database
        */
        actions <- actionsRef.get
        _ <- ZIO.foreach(actions)(a => console.printLine(a))
      } yield ()
  }

  // layer
  val live: ZLayer[Clock & Console, Nothing, Logger] = ZLayer {
    for {
      actionsRef <- Ref.make(Chunk.empty[GameActions])
      clock <- ZIO.service[Clock]
      console <- ZIO.service[Console]
    } yield LoggerImpl(clock, console, actionsRef)
  }

  // accessors
  def playerMoved(pid: PlayerId, position: Int, action: PlayerActions, stage: GameStages, wager: Int = 0): ZIO[Logger, Throwable, Unit] =
    ZIO.serviceWithZIO(_.playerMoved(pid, position, action, stage, wager))
    
  def dealerMoved(action: DealerActions, cards: Chunk[Card]): ZIO[Logger, Throwable, Unit] =
    ZIO.serviceWithZIO(_.dealerMoved(action, cards))

  def playerEvaluated(pid: PlayerId, result: GameResults, topCombination: PokerCombinations, cards: Chunk[Card], profit: Int): ZIO[Logger, Throwable, Unit] =
    ZIO.serviceWithZIO(_.playerEvaluated(pid, result, topCombination, cards, profit))
    
  def saveAndClear(): ZIO[Logger, Throwable, Unit] =
    ZIO.serviceWithZIO(_.saveAndClear())
}
