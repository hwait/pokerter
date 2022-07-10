# Simple poker simulator ***
This is my first **ZIO 2** experience. I hope I've made not many imperative mistakes. 

*** *`Project currently on pause because of RockTheJVM`* [ZIO course](https://rockthejvm.com/p/zio) !

### Right now it able to:
- play N games for M players in parallel
- deal hole cards to players
- move players position
- visualize game info and player's cards
- deal cards for Holdem and Omaha (2 or 4 that's the difference:)
- evaluates hands and finds the best combination for Holdem
- keeps player balance
- game logic: stages, cards deals, player interactions - check/call only
- State for game events - inmemory only

### Future features:
- State for game events - DB store
- tests (yes, it was not TDD)
- Player possible actions depends on events and the balance
- Make game logic injectable
- Make different strategies for player inside the game logic stage (start from "Always Check/Call" strategy)

## The structure corresponds to the domain:

> Table 
- Top module in hierarchy. 
- Can run N games
- initiates position shift
- initiates cards shuffle in the `Deck`
- deals hole cards
- evaluates game result

> Game
- plays on a Table. 
- main properties is defined in `GameConfig`
- creates and rules the players


> Player 
- keeps hole cards and position
- rules own bankroll
- has a strategy how to play - TBD

> Deck
- shuffles the deck
- provides a card to anyone interested

> Card 
- simple structure with `nominal` and `suit`. 
- can be pretty printed to console
- can be constructed from index. The cards are sorted from 2 to Ace and by suit in the order: Club -> Spades-> Hearts -> Diamonds

> Logger
- service provided as a Layer
- keeps game events
- writes them to the console
- writes them to database - TBD