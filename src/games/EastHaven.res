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

  module StandardBoard = {
    @react.component
    let make = (~setRef) => {
      <React.Fragment>
        <div className="flex flex-row">
          <div
            key={Stock->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Packer.Stock))}
            className=" bg-white opacity-10  rounded w-14 h-20 mr-20"
          />
          <div className="flex flex-row gap-3 ml-10">
            {[[], [], [], []]
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Foundation(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
                className=" bg-white opacity-10  rounded w-14 h-20"
              />
            })
            ->React.array}
          </div>
        </div>
        <div />
        <div className="flex flex-row gap-3 mt-5">
          {[[], [], [], [], [], [], []]
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Pile(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
              className=" bg-black opacity-20   rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
      </React.Fragment>
    }
  }

  module Board = StandardBoard
})
