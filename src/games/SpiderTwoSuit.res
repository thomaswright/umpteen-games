open Common

module GameRules: GameBase.GameRules = {
  include Spider.SpiderRules

  let initiateGame = () => {
    let shuffledDeck =
      Array.concatMany(
        [],
        [
          Card.getOneSuitDeck(0, Spades, true),
          Card.getOneSuitDeck(1, Spades, true),
          Card.getOneSuitDeck(2, Spades, true),
          Card.getOneSuitDeck(3, Spades, true),
          Card.getOneSuitDeck(4, Hearts, true),
          Card.getOneSuitDeck(5, Hearts, true),
          Card.getOneSuitDeck(6, Hearts, true),
          Card.getOneSuitDeck(7, Hearts, true),
        ],
      )->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
        ]->flipLastUp,
        foundations: [[], [], [], [], [], [], [], []],
        stock: [
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
        ],
      },
    )
  }
}

module Game = GameBase.Create(GameRules)
