package cards.logic
import zio._
import cards.domain._
import cards.services.Logger
import math.Ordered.orderingToOrdered
import java.io.IOException

/**
 * Represents Table with dealer and players
 *
 * @param tableId
 *   table ID
 * @param gameConfig
 *   game config from enum [[cards.domain.GameConfigs]]
 * @param gameRef
 *   current game state [[cards.logic.Game]]
 * @param playersN
 *   TODO: does invested clears every round? players number on the table
 * @param playersRef
 *   players info [[cards.logic.Player]]
 * @param deck
 *   cards deck service [[cards.logic.Deck]]
 * @param console
 *   the Console service
 */
case class Table(
  tableId: TableId,
  gameConfig: GameConfigs,
  gameRef: Ref[Game],
  playersN: Int,
  playersRef: Ref[Chunk[Player]],
  deck: Deck,
  console: Console
) {

  /**
   * Processes one round of play. Recurrently calls until players consesus met
   *
   * @param id
   *   first player position
   *
   *   - Only active players make move
   *   - when consensus met game state clears
   */
  def playRound(id: Int): ZIO[Logger & Clock & Console, Throwable, Unit] =
    for {
      game        <- gameRef.get
      players     <- playersRef.get
      status      <- players(id).statusRef.get
      _           <- ZIO.when(status == PlayerStatus.Active)(game.playerMove(players(id)))
      isNextRound <- game.isNextRound
      _ <- if (isNextRound) playRound(if (id >= playersN - 1) 0 else id + 1)
           else game.nextRound()
    } yield ()

  /**
   * Compares players combinations and defines losers and winners. 
   * 
   * (1) winners detects first
   * (2) we have to add prize to winners (loser's already subtracted wagers from their banks so no need to update)
   * (3) log results for every player in game.setPlayerResult
   */
  val evaluatePlayers: ZIO[Logger, Throwable, Unit] = {

    /** Comparator of two BestPlayerCombinations */
    def combComparator(a: BestPlayerCombination, b: BestPlayerCombination): Compares =
      if (a.comb.ordinal > b.comb.ordinal) Compares.This
      else if (a.comb.ordinal < b.comb.ordinal) Compares.That
      else if (a.combNominals > b.combNominals) Compares.This
      else if (a.combNominals < b.combNominals) Compares.That
      else Compares.Equal
    for {
      players <- playersRef.get
      game    <- gameRef.get
      pot     <- game.potRef.get
      winners <- ZIO.foldLeft(players)(Chunk.empty[Player]) { (w, p) => // (1)
                   for {
                     bestComb  <- if (w.isEmpty) ZIO.succeed(BestPlayerCombination()) else w.head.combRef.get
                     candidate <- p.combRef.get
                     invested  <- p.investedRef.get
                     newBest =
                       // if no winners yet any combination wins:
                       if (w.isEmpty) w :+ p  
                       // if current winner wins no changes:
                       else if (combComparator(bestComb, candidate) == Compares.This) w 
                       // if current player wins we have new winners:
                       else if (combComparator(bestComb, candidate) == Compares.That) Chunk(p) 
                       // if player has the same combination as the winner that means TIED WINNERS and ther prize will be divided:
                       else w :+ p  
                   } yield newBest
                 }
      _ <- ZIO.foreach(winners)(w => w.won(pot / winners.size)) // (2)
      _ <- ZIO.foreach(players) { p =>  // (3)
             if (winners.contains(p))
               game.setPlayerResult(p, GameResults.Won, winners.size)
             else
               game.setPlayerResult(p, GameResults.Lost, 1)
           }
    } yield ()
  }

  /**
   * Plays one game - Main play logic
   * 
   * (1) dealer deals hole cards
   * (2) players make moves and met consensus
   * (3) dealer deals cards on the board
   * (4) players evaluate by finding best combination
   * (5) evaluate game result
   * (6) players prepare for the next game
   *    - move position by one step clockwise
   *    - clear state (p.renew)
   */
  val play: ZIO[Clock & Console & Logger, Throwable, Unit] =
    for {
      logger  <- ZIO.service[Logger]
      game    <- gameRef.get
      newGame <- Game(tableId, game.gid + 1, gameConfig, playersN, game.blindes)
      _       <- gameRef.set(newGame)
      _       <- deck.shuffle
      players <- playersRef.get
      // pid: PlayerId, n: Int, position: Int, status: PlayerStatus, bank: Int
      _ <- ZIO.foreach(players)(p => p.addCard(deck.oneCard)).repeatN(gameConfig.cardsInHand - 1) // (1)
      // _     <- status
      _     <- playRound(0) // (2) Preflop
      _     <- newGame.addCard(deck.oneCard).repeatN(2) // (3)
      _     <- playRound(0) // (2) Flop
      _     <- newGame.addCard(deck.oneCard) // (3)
      _     <- playRound(0) // (2) Turn
      _     <- newGame.addCard(deck.oneCard) // (3)
      _     <- playRound(0) // (2) River
      board <- newGame.boardRef.get
      _     <- ZIO.foreach(players)(p => p.evaluate(board)) // (4)
      _     <- evaluatePlayers // (5)
      newPlayers <- ZIO.foreach((players.last +: players.dropRight(1)).zipWithIndexFrom(0))((p, i) => // (6)
                      p.renew *> ZIO.succeed(p.copy(position = i))
                    )
      _ <- playersRef.set(newPlayers)
    } yield ()

  /**  Plays multiple games 
   * @param gamesN how many games play on this table
  */
  def playN(gamesN: Int) = play.repeatN(gamesN - 1)

  /** Writes table state to the console */ 
  val status: ZIO[Console, IOException, Unit] =
    for {
      game    <- gameRef.get
      players <- playersRef.get
      _ <-
        console.printLine(
          s"[T$tableId / G${game.gid}] ${gameConfig.limit} ${gameConfig.name} ${game.blindes.sb}/${game.blindes.bb} for $playersN pls:"
        )
      _ <- ZIO.foreach(players)(p => p.status)
    } yield ()
}

/** Factory for [[cards.logic.Table]] instances. 
 * 
 * @param tableId
 *   table ID
 * @param blindes
 *   initial blindes and ante
 * @param gameConfig
 *   game config from enum [[cards.domain.GameConfigs]]
 * @param playersN
 *   players number on the table
 */
object Table {
  def apply(
    tableId: TableId,
    blindes: Blindes,
    gameConfig: GameConfigs,
    playersN: Int
  ): ZIO[Logger & Console & Random, Throwable, Table] =
    for {
      console <- ZIO.service[Console]
      game    <- Game(tableId, 0, gameConfig, playersN, blindes)

      gameRef <- Ref.make(game)

      players <- ZIO.foreach(Chunk.range(0, playersN)) { n =>
                   Player(tableId * 10 + n, n, n, PlayerStatus.Active, blindes.bb * 100)
                 }

      playersRef <- Ref.make(players)
      deck       <- Deck(gameConfig.cardsInDeck)
    } yield Table(tableId, gameConfig, gameRef, playersN, playersRef, deck, console)
}
