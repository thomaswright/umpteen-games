open Common

module Base = Packer.Make({
  let spec: Packer.spec = {
    drop: AltSuit,
    drag: AltSuit,
    size: AnySize,
    depot: AnyDepot,
    foundation: ByOne,
  }
})

let flipLastUp = (piles: array<array<Card.sides>>) =>
  piles->Array.map(pile => pile->ArrayAux.updateLast(v => {...v, hidden: false}))

module Game = GameBase.Create({
  include Base

  let winCheck = Spider.SpiderRules.winCheck

  let stockRules = Spider.SpiderRules.stockRules

  let forEachSpace = Base.makeForEachSpace(~stockRules)

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, true)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
        ]->flipLastUp,
        foundations: [[], [], [], []],
        stock: [
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
        ],
        free: [],
        waste: [],
      },
    )
  }

  module Board = Spider.SpiderRules.StandardBoard
})
