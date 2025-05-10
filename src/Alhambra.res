open Common

module Game = GameBase.Create({
  include Bases.Alhambra

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany([], [Card.getDeck(0, false), Card.getDeck(1, false)])->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    deckToDeal :=
      deckToDeal.contents->Array.filterMap(v => {
        if (v.card.rank == RA || v.card.rank == RK) && v.card.deck == 0 {
          None
        } else {
          Some(v)
        }
      })

    let free = [
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
    ]

    let stock = [deckToDeal.contents->Card.hideAfter(0)]

    (
      shuffledDeck,
      {
        tableau: [],
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
        foundations2: [
          [
            {
              card: {
                suit: Spades,
                rank: RK,
                deck: 0,
              },
              hidden: false,
            },
          ],
          [
            {
              card: {
                suit: Hearts,
                rank: RK,
                deck: 0,
              },
              hidden: false,
            },
          ],
          [
            {
              card: {
                suit: Diamonds,
                rank: RK,
                deck: 0,
              },
              hidden: false,
            },
          ],
          [
            {
              card: {
                suit: Clubs,
                rank: RK,
                deck: 0,
              },
              hidden: false,
            },
          ],
        ],
        free,
        stock,
        waste: [],
      },
    )
  }

  let forEachSpace = Bases.Alhambra.makeForEachSpace(
    ~freeRules=Rules.FreeCell.freeRules,
    ~wasteRules=Rules.WasteRotation.stackedBuildWasteRules,
    ~stockRules=Rules.WasteRotation.stockRules,
    ~stockBaseRules=Rules.WasteRotation.stockBaseRules,
  )

  module Board = Boards.SWFFR
})
