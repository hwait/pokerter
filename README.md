# Simple poker simulator
This is my first **ZIO 2** experience. I hope I've made not many imperative mistakes. 

### Right now it able to:
- play N games for M players
- deal hole cards to players
- move players position
- visualize game info and player's cards
- deal cards for Holdem and Omaha (2 or 4 that's the difference:)

### Future features:
- tests (yes, it was not TDD)
- hands evaluator
- State for game events
- Player balance
- Player possible actions depends on events and the balance
- Game logic: stages, cards deals, player interactions
- Make game logic injectable
- Make different strategies for player inside the game logic stage (start from "Always Check/Call" strategy)

## The structure corresponds to the domain:

> Table 
- Top module in hierarchy. 
- Can run N games

> Game
- plays on a Table. 
- main properties is defined in `GameConfig`
- creates and rules the players
- initiates cards shuffle in the `Deck`
- initiates position shift
- deals hole cards

> Player 
- keeps hole cards and position
- rules own bankroll - TBD
- has a strategy how to play - TBD

> Deck
- shuffles the deck
- provides a card to anyone interested

> Card 
- simple structure with `nominal` and `suit`. 
- can be pretty printed to console
- can be constructed from index. The cards are sorted from 2 to Ace and by suit in the order: Club -> Spades-> Hearts -> Diamonds
