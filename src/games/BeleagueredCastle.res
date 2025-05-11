open Common
open Packer

module Game = GameBase.Create({
  include Bases.Diplomat

  let forEachSpace = makeForEachSpace()

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled
    let deckToDeal = ref(shuffledDeck)

    deckToDeal :=
      deckToDeal.contents->Array.filterMap(v => {
        if v.card.rank == RA {
          None
        } else {
          Some(v)
        }
      })

    let tableau = [
      deckToDeal->ArrayAux.popN(6),
      deckToDeal->ArrayAux.popN(6),
      deckToDeal->ArrayAux.popN(6),
      deckToDeal->ArrayAux.popN(6),
      deckToDeal->ArrayAux.popN(6),
      deckToDeal->ArrayAux.popN(6),
      deckToDeal->ArrayAux.popN(6),
      deckToDeal->ArrayAux.popN(6),
    ]

    (
      shuffledDeck,
      {
        tableau,
        foundations: [
          [
            {
              card: {
                suit: Spades,
                rank: RA,
                deck: 0,
              },
              hidden: false,
            },
          ],
          [
            {
              card: {
                suit: Hearts,
                rank: RA,
                deck: 0,
              },
              hidden: false,
            },
          ],
          [
            {
              card: {
                suit: Diamonds,
                rank: RA,
                deck: 0,
              },
              hidden: false,
            },
          ],
          [
            {
              card: {
                suit: Clubs,
                rank: RA,
                deck: 0,
              },
              hidden: false,
            },
          ],
        ],
        foundations2: [],
        free: [],
        waste: [],
        stock: [],
      },
    )
  }

  module Board = Boards.FT
})
