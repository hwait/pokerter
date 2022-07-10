package cards.logic

import zio._
import java.io.IOException
import cards.domain._
import cards.services.Logger

/**
 * Represents store for Game data
 *
 * @param pid
 *   player ID
 * @param name
 *   player name
 * @param position
 *   player position
 * @param statusRef
 *   player status from enum [[cards.domain.PlayerStatus]]
 * @param bankRef
 *   player's bank size in cents
 * @param investedRef
 *   TODO: does invested clears every round? total player's stake in cents
 * @param holeCardsRef
 *   player's hole cards
 * @param combRef
 *   best player's combination BestPlayerCombination
 * @param console
 *   the Console service
 */
case class Player(
  pid: PlayerId,
  name: String,
  position: Int,
  statusRef: Ref[PlayerStatus],
  bankRef: Ref[Int],
  investedRef: Ref[Int],
  holeCardsRef: Ref[Chunk[Card]],
  combRef: Ref[BestPlayerCombination],
  console: Console
) {

  /** Adds hole card to player */
  def addCard(card: UIO[Card]): UIO[Unit] =
    for {
      c <- card
      _ <- holeCardsRef.update(_ :+ c)
    } yield ()

  /** Updates the player's bank if won */
  def won(pot: Int): UIO[Unit] =
    for {
      _ <- bankRef.update(_ + pot)
    } yield ()

  /** Prepares player for the next game */
  val renew: UIO[Unit] =
    for {
      _ <- statusRef.set(PlayerStatus.Active)
      _ <- investedRef.set(0)
      _ <- holeCardsRef.set(Chunk.empty[Card])
    } yield ()

  /**
   * Processes any player's moves with money involved. These such actions as
   * Bet, Call, Raise and put blindes
   *
   * @param biggestWager
   *   current biggest wager
   * @param wager
   *   player's wager
   * @param action
   *   player action from enum [[cards.domain.PlayerActions]]
   * @param stage
   *   game stage from enum [[cards.domain.GameStages]]
   * @return
   *   PlayerAction prepared for logging
   *
   * To get amount to subtract from bank:
   *   - subtract money already wagered in this round
   *   - if bank is less then amount then amount is the bank size
   */
  def bet(biggestWager: Int, wager: Int, action: PlayerActions, stage: GameStages): URIO[Clock, PlayerAction] =
    for {
      clock    <- ZIO.service[Clock]
      dt       <- clock.nanoTime
      bank     <- bankRef.get
      invested <- investedRef.get
      // _ <- console.printLine(s"$name on $position with ($bank): $action $wager against $biggestWager, $invested already invested ---> ")
      toClose = if (bank >= wager - invested) wager - invested else bank
      _      <- bankRef.set(bank - toClose)
      _      <- investedRef.set(invested + toClose)
      // _ <- console.printLine(s"$toClose wagered, so total invested ${invested + toClose}, new bank (${bank - toClose})")

    } yield PlayerAction(pid, name, position, stage, action, toClose, bank - toClose, dt)

  /**
   * Processes any player's moves without money involved. These such actions as
   * Check and Fold
   *
   * @param action
   *   player action from enum [[cards.domain.PlayerActions]]
   * @param stage
   *   game stage from enum [[cards.domain.GameStages]]
   * @return
   *   PlayerAction prepared for logging
   */
  def noBet(action: PlayerActions, stage: GameStages): URIO[Clock, PlayerAction] =
    for {
      clock    <- ZIO.service[Clock]
      dt       <- clock.nanoTime
      bank     <- bankRef.get
      invested <- investedRef.get
      // _ <- console.printLine(s"$name on $position with ($bank): $action, $invested invested")
    } yield PlayerAction(pid, name, position, stage, action, 0, bank, dt)

  def getAllCards(board: Chunk[Card]): UIO[Chunk[Card]] =
    holeCardsRef.get.map(cs => (cs ++ board).sortWith((a, b) => a.nominal > b.nominal))

  /**
   * Entry point for player's move. These such actions as Check and Fold
   *
   * @param gameStageN
   *   game stage ordinal
   * @param actions
   *   list of current actions made by ACTIVE players
   * @param biggestWager
   *   biggest wager made in round
   * @param blindes
   *   current blindes
   * @param board
   *   current board
   * @return
   *   PlayerAction prepared for logging
   *
   * Hole cards combine with board cards and provides on Flop+ stages
   */
  def move(
    gameStageN: Int,
    actions: Chunk[PlayerAction],
    biggestWager: Int,
    blindes: Blindes,
    board: Chunk[Card]
  ): URIO[Clock, PlayerAction] =
    for {
      cards     <- getAllCards(board)
      gameStage <- ZIO.succeed(GameStages.fromOrdinal(gameStageN))
      pa <- gameStage match {
              case GameStages.Preflop => preflop(actions, biggestWager, blindes)
              case GameStages.Flop    => flop(actions, biggestWager, cards)
              case GameStages.Turn    => turn(actions, biggestWager, cards)
              case GameStages.River   => river(actions, biggestWager, cards)
            }
    } yield pa

  /**
   * Preflop decision logic
   *
   * @param actions
   *   list of current actions made by ACTIVE players
   * @param biggestWager
   *   biggest wager made in round
   * @param blindes
   *   current blindes
   * @return
   *   PlayerAction prepared for logging
   *
   *   - Player on position 0 (SB) if no wagers made and nothing invested then
   *     puts small blind
   *   - Player on position 1 (BB) if SB already put wager but BB has not then
   *     puts big blind
   *   - Player on other situations make own decision by TODO: PLAYER LOGIC
   *     Current logic is check/call
   */
  def preflop(actions: Chunk[PlayerAction], biggestWager: Int, blindes: Blindes): URIO[Clock, PlayerAction] =
    for {
      invested <- investedRef.get
      pa <- (biggestWager, invested, position) match
              case (0, 0, 0) =>
                bet(0, blindes.sb, PlayerActions.Bet, GameStages.Preflop)
              case (blindes.sb, 0, 1) =>
                bet(blindes.sb, blindes.bb, PlayerActions.Bet, GameStages.Preflop)
              case _ =>
                bet(biggestWager, biggestWager, PlayerActions.Call, GameStages.Preflop)
    } yield pa

  /**
   * Flop decision logic
   *
   * @param actions
   *   list of current actions made by ACTIVE players
   * @param biggestWager
   *   biggest wager made in round
   * @param cards
   *   all cards available for make combinations
   * @return
   *   PlayerAction prepared for logging
   *
   * Current logic is simple check
   *
   * TODO: PLAYER LOGIC. For player logic the current pot is necessary
   */
  def flop(actions: Chunk[PlayerAction], biggestWager: Int, cards: Chunk[Card]): URIO[Clock, PlayerAction] =
    for {
      // _ <- console.printLine(s"[FLOP] $name on $position ")
      pa <- noBet(PlayerActions.Check, GameStages.Flop)
    } yield pa

  /**
   * Turn decision logic
   *
   * @param actions
   *   list of current actions made by ACTIVE players
   * @param biggestWager
   *   biggest wager made in round
   * @param cards
   *   all cards available for make combinations
   * @return
   *   PlayerAction prepared for logging
   *
   * Current logic is simple check
   *
   * TODO: PLAYER LOGIC. For player logic the current pot is necessary
   */
  def turn(actions: Chunk[PlayerAction], biggestWager: Int, cards: Chunk[Card]): URIO[Clock, PlayerAction] =
    for {
      // _ <- console.printLine(s"[TURN] $name on $position ")
      pa <- noBet(PlayerActions.Check, GameStages.Turn)
    } yield pa

  /**
   * River decision logic
   *
   * @param actions
   *   list of current actions made by ACTIVE players
   * @param biggestWager
   *   biggest wager made in round
   * @param cards
   *   all cards available for make combinations
   * @return
   *   PlayerAction prepared for logging
   *
   * Current logic is simple check
   *
   * TODO: PLAYER LOGIC. For player logic the current pot is necessary
   */
  def river(actions: Chunk[PlayerAction], biggestWager: Int, cards: Chunk[Card]): URIO[Clock, PlayerAction] =
    for {
      // _ <- console.printLine(s"[RIVER] $name on $position ")
      pa <- noBet(PlayerActions.Check, GameStages.River)
    } yield pa

  /**
   * Return top combination with description
   *
   * @param board
   *   current board
   * @return
   *   top combination [[cards.domain.BestPlayerCombination]]
   */
  def evaluate(board: Chunk[Card]): UIO[BestPlayerCombination] =
    for {
      cards                 <- getAllCards(board)
      bestPlayerCombination <- ZIO.succeed(findCombination(cards))
      _                     <- combRef.set(bestPlayerCombination)
    } yield bestPlayerCombination

  /**
   * Find top combination
   *
   * @param cards
   *   all cards available for make combinations
   * @return
   *   top combination [[cards.domain.BestPlayerCombination]]
   *
   * Evaluates from top to bottom via pattern matching
   */
  def findCombination(cards: Chunk[Card]): BestPlayerCombination = {

    /**
     * Straight comparator. If min and max difference between neigbours equal to
     * one
     */
    def isStraight(cards: Chunk[Card]) = {
      val list = cards.zip(cards.tail).map((x, y) => x.nominal - y.nominal)
      val max  = list.max
      max == list.min && max == 1
    }

    /**
     * Ordering evaluator of combinations with nominals. Like pairs, sets and so
     * on
     *
     * @param k
     *   card nominal
     * @param v
     *   count of cards by the nominal
     * @return
     *   numerical score
     */
    def orderF(k: Int, v: Int): Int = v * 100 + k

    /**
     * Counts max number of same suit For Flush detection
     *
     * @return
     *   tuple (suit, count)
     */
    val maxSuits: (Int, Int) = cards.groupMapReduce(c => c.suit)(_ => 1)(_ + _).maxBy(_._2)

    /**
     * Finds top Flush
     *
     * @return
     *   Option with 5 cards if flush has found and None otherwise
     */
    val flush: Option[Chunk[Card]] =
      if (maxSuits._2 >= 5)
        Some(cards.filter(c => c.suit == maxSuits._1).take(5))
      else None

    /**
     * Finds all straigts We need all starghts because of straight flash
     * possibility where the top combination can contain lower straght
     *
     * @return
     *   List of all starights has found ordered descently
     */
    val straights: List[Chunk[Card]] =
      if (cards(0).nominal == 12)
        (cards :+ cards.head)
          .sliding(5, 1)
          .filter(isStraight)
          .toList // For straight search we have to copy Ace to the end of the chunk
      else cards.sliding(5, 1).filter(isStraight).toList

    /**
     * Straight flash detection
     *
     * @return
     *   Option with straight flash
     */
    val straightFlash: Option[Chunk[Card]] =
      straights.find(s => s(0).suit == maxSuits._1 && maxSuits._2 >= 5) // first SF found

    /**
     * Top doubled nominal
     *
     * @return
     *   tuple (nominal, count)
     */
    val max1Nominal: (Int, Int) = cards.groupMapReduce(c => c.nominal)(_ => 1)(_ + _).maxBy(orderF)

    /**
     * Cards with the top doubled nominal filtered out
     *
     * @return
     *   tuple (nominal, count)
     */
    val restCards: Chunk[Card] = cards.filterNot(c => c.nominal == max1Nominal._1)

    /**
     * Second top doubled nominal
     *
     * @return
     *   tuple (nominal, count)
     */
    val max2Nominal: (Int, Int) = restCards.groupMapReduce(c => c.nominal)(_ => 1)(_ + _).maxBy(orderF)

    // println(s"${cards.mkString("[","","]")}")
    // println((maxSuits, straights))
    // println((max1Nominal, max2Nominal))

    val comb: BestPlayerCombination = (straightFlash, flush, straights, max1Nominal, max2Nominal) match
      case (Some(sf), _, _, _, _) => // Straight Flash
        if (sf(0).nominal == 12) BestPlayerCombination(PokerCombinations.RoyalFlush, Chunk(0), sf, cards)
        else BestPlayerCombination(PokerCombinations.StraightFlush, Chunk(sf(0).nominal), sf, cards)
      case (None, None, Nil, (nom1, 4), _) => // Quads
        val valued = cards.filter(c => c.nominal == nom1) ++ restCards.take(1)
        BestPlayerCombination(PokerCombinations.Quads, Chunk(nom1), valued, cards)
      case (None, None, Nil, (nom1, 3), (nom2, 2)) => // Full house
        val valued = cards.filter(c => c.nominal == nom1) ++ cards.filter(c => c.nominal == nom2)
        BestPlayerCombination(PokerCombinations.FullHouse, Chunk(nom1, nom2), valued, cards)
      case (None, Some(f), Nil, (nom1, _), _) => // Flush
        BestPlayerCombination(PokerCombinations.Flush, Chunk(f.head.nominal), f, cards)
      case (None, None, straight :: _, (nom1, _), _) => // Straight
        BestPlayerCombination(PokerCombinations.Straight, Chunk(straight.head.nominal), straight, cards)
      case (None, None, Nil, (nom1, 3), _) => // Set
        val valued = cards.filter(c => c.nominal == nom1) ++ restCards.take(2)
        BestPlayerCombination(PokerCombinations.Set, Chunk(nom1) ++ restCards.take(2).map(_.nominal), valued, cards)
      case (None, None, Nil, (nom1, 2), (nom2, 2)) => // Two pairs
        val valued =
          cards.filter(c => c.nominal == nom1) ++
            cards.filter(c => c.nominal == nom2) ++
            cards.filterNot(c => c.nominal == nom1 || c.nominal == nom2).take(1)
        BestPlayerCombination(
          PokerCombinations.TwoPairs,
          Chunk(nom1, nom2) ++ cards.map(_.nominal).filterNot(n => n == nom1 || n == nom2).take(1),
          valued,
          cards
        )
      case (None, None, Nil, (nom1, 2), (nom2, 1)) => // Pair
        val valued = cards.filter(c => c.nominal == nom1) ++ restCards.take(3)
        BestPlayerCombination(PokerCombinations.Pair, Chunk(nom1) ++ restCards.take(3).map(_.nominal), valued, cards)
      case _ => // High card
        val valued = cards.take(5)
        BestPlayerCombination(PokerCombinations.HighCard, cards.map(_.nominal), valued, cards)
    // println(comb)
    comb
  }

  /** Writes player state to the console */
  val status: ZIO[Console, IOException, Unit] = {
    def positionText(pos: Int) = pos match
      case 0 => " on SB"
      case 1 => " on BB"
      // case 2 => " on BTN"
      case _ => s" on $pos "
    for {
      cards <- holeCardsRef.get
      _     <- console.printLine(s"$pid: $name${positionText(position)} ${cards.mkString("[", ",", "]")}")
    } yield ()
  }
}

/**
 * Factory for [[cards.logic.Player]] instances.
 *
 * @param pid
 *   player ID
 * @param n
 *   player number on table
 * @param position
 *   player position on table
 * @param status
 *   player status from enum [[cards.domain.PlayerStatus]]
 * @param bank
 *   player's initial bank size in cents
 */
object Player {
  def apply(pid: PlayerId, n: Int, position: Int, status: PlayerStatus, bank: Int): URIO[Console, Player] =
    for {
      statusRef    <- Ref.make(status)
      bankRef      <- Ref.make(bank)
      investedRef  <- Ref.make(0)
      holeCardsRef <- Ref.make(Chunk.empty[Card])
      combRef      <- Ref.make(BestPlayerCombination())
      console      <- ZIO.service[Console]
    } yield Player(pid, s"player$n", position, statusRef, bankRef, investedRef, holeCardsRef, combRef, console)
}
