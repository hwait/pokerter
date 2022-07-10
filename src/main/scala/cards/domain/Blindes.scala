package cards.domain

/** Info for Preflop stage
 *
 *  @param sb Small blind size.
 *  @param bb Big blind size.
 *  @param ante Ante size if applicable.
 */
final case class Blindes(sb: Int, bb: Int, ante: Int)
