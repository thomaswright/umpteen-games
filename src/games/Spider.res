open Webapi.Dom
open Common

module SpiderRules = {
  @decco
  type space = Card(Card.card) | Foundation(int) | Pile(int) | Stock

  let getSpace = element => {
    switch element->Element.id->Js.Json.parseExn->space_decode {
    | Ok(d) => Some(d)
    | _ => None
    }
  }

  let spaceToString = space => {
    space->space_encode->Js.Json.stringify
  }
  @decco
  type deck = array<Card.sides>

  type dragPile = array<Card.sides>

  @decco
  type game = {
    piles: array<array<Card.sides>>,
    foundations: array<array<Card.sides>>,
    stock: array<array<Card.sides>>,
  }

  type movableSpace = GameBase.movableSpace<game, space, dragPile>
  type staticSpace = GameBase.staticSpace<game, dragPile>

  let flipLastUp = (piles: array<array<Card.sides>>) =>
    piles->Array.map(pile => pile->ArrayAux.updateLast(v => {...v, hidden: false}))

  let winCheck = (game: game) => {
    game.piles->Array.every(pile => pile->Array.length == 0) &&
      game.stock->Array.every(stockGroup => stockGroup->Array.length == 0)
  }

  let removeDragFromGame = (game: game, dragPile: dragPile) => {
    let dragPileSet = dragPile->Set.fromArray
    let removeDragPile = x =>
      x->Array.filter(sCard => {
        !(dragPileSet->Set.has(sCard))
      })

    {
      foundations: game.foundations->Array.map(removeDragPile),
      piles: game.piles->Array.map(removeDragPile),
      stock: game.stock->Array.map(removeDragPile),
    }
  }

  let applyLiftToDragPile = (dragPile: dragPile, lift) => {
    dragPile->Array.forEachWithIndex((v, j) => {
      lift(Card(v.card), j)
    })
  }

  let applyMoveToDragPile = (dragPile: dragPile, move) => {
    dragPile->Array.forEachWithIndex((v, j) => {
      move(Card(v.card), 0, j * 20)
    })
  }

  let pileBaseRules = (i): staticSpace => {
    {
      droppedUpon: (game, dragPile) => {
        let noChildren = game.piles->Array.getUnsafe(i)->Array.length == 0

        if noChildren {
          Some({
            ...game,
            piles: game.piles->ArrayAux.update(i, _ => dragPile)->flipLastUp,
          })
        } else {
          None
        }
      },
      autoProgress: Accept,
      onClick: _ => None,
    }
  }

  let pileRules = (_game, pile, card, i, j): movableSpace => {
    let isLast = j == pile->Array.length - 1

    {
      locationAdjustment: {
        x: 0,
        y: j * 20,
        z: j + 1,
      },
      baseSpace: Pile(i),
      dragPile: () => {
        let dragPile = pile->Array.sliceToEnd(~start=j)
        if dragPile->GameCommons.decValidation {
          Some(dragPile)
        } else {
          None
        }
      },
      autoProgress: () => {
        if isLast {
          SendOrAccept([card])
        } else {
          DoNothing
        }
      },
      droppedUpon: (game, dragPile) => {
        let dragPileBase = dragPile->Array.getUnsafe(0)

        if isLast && Card.rankIsAbove(card, dragPileBase) {
          Some({
            ...game,
            piles: game.piles
            ->Array.map(stack => {
              stack->ArrayAux.insertAfter(card, dragPile)
            })
            ->flipLastUp,
          })
        } else {
          None
        }
      },
      onClick: _ => None,
      onStateChange: element => {
        if card.hidden {
          Card.hide(element)
        } else {
          Card.show(element)
        }
      },
    }
  }

  let foundationBaseRules = (i): staticSpace => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        let fullStack = dragPile->Array.length == 13
        let noChildren = game.foundations->Array.getUnsafe(i)->Array.length == 0
        let valid = dragPile->GameCommons.decValidation
        if fullStack && noChildren && valid {
          Some({
            ...game,
            foundations: game.foundations->ArrayAux.update(i, _ => dragPile),
            piles: game.piles->flipLastUp,
          })
        } else {
          None
        }
      },
      onClick: _ => None,
    }
  }

  let foundationRules = (i, j): movableSpace => {
    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: j + 1,
      },
      baseSpace: Foundation(i),
      dragPile: () => {
        None
      },
      autoProgress: () => DoNothing,
      droppedUpon: (_game, _dragPile) => {
        None
      },
      onClick: _ => None,
      onStateChange: element => {
        Card.show(element)
      },
    }
  }

  let stockGroupRules = (_game, _card, i, j): movableSpace => {
    {
      locationAdjustment: {
        x: i * 20,
        y: 0,
        z: i * 10 + j + 1,
      },
      baseSpace: Stock,
      dragPile: () => {
        None
      },
      autoProgress: () => DoNothing,
      droppedUpon: (_game, _dragPile) => {
        None
      },
      onClick: game => {
        game.stock
        ->Common.ArrayAux.getLast
        ->Option.map(stockGroup => {
          {
            ...game,
            piles: game.piles
            ->Array.mapWithIndex((pile, i) => {
              stockGroup->Array.get(i)->Option.mapOr(pile, v => Array.concat(pile, [v]))
            })
            ->flipLastUp,
            stock: game.stock->Array.slice(~start=0, ~end=game.stock->Array.length - 1),
          }
        })
      },
      onStateChange: element => {
        Card.hide(element)
      },
    }
  }

  let forEachSpace: GameBase.forEachSpace<game, space, dragPile> = (game: game, f) => {
    game.piles->Array.forEachWithIndex((pile, i) => {
      f(Pile(i), pileBaseRules(i)->GameBase.Static)

      pile->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), pileRules(game, pile, card, i, j)->Movable)
      })
    })

    game.foundations->Array.forEachWithIndex((foundation, i) => {
      f(Foundation(i), foundationBaseRules(i)->Static)

      foundation->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), foundationRules(i, j)->Movable)
      })
    })

    game.stock->Array.forEachWithIndex((stockGroup, i) => {
      stockGroup->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), stockGroupRules(game, card, i, j)->Movable)
      })
    })
  }

  module StandardBoard = {
    @react.component
    let make = (
      ~setRef,
      ~onMouseDown as _,
      ~setGame as _,
      ~moveToState as _,
      ~autoProgress as _,
      ~game as _,
      ~undo as _,
      ~isWin as _,
      ~onClick as _,
    ) => {
      <React.Fragment>
        <div className="flex flex-row">
          <div
            key={Stock->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Stock))}
            className=" bg-white opacity-10  rounded w-14 h-20 mr-20"
          />
          <div className="flex flex-row gap-3 ml-10">
            {[[], [], [], [], [], [], [], []]
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
          {[[], [], [], [], [], [], [], [], [], []]
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

  module AllCards = {
    @react.component
    let make = (~setRef, ~onMouseDown, ~deck) => {
      <React.Fragment>
        {deck
        ->Array.map(card => {
          <Card.Display
            card={card}
            key={Card(card.card)->spaceToString}
            id={Card(card.card)->spaceToString}
            cardRef={ReactDOM.Ref.callbackDomRef(setRef(Card(card.card)))}
            onMouseDown={onMouseDown}
          />
        })
        ->React.array}
      </React.Fragment>
    }
  }
}

module OneSuit = GameBase.Create({
  include SpiderRules

  let initiateGame = () => {
    let shuffledDeck =
      Array.concatMany(
        [],
        [
          Card.getOneSuitDeck(0, Spades, true),
          Card.getOneSuitDeck(1, Spades, true),
          Card.getOneSuitDeck(2, Spades, true),
          Card.getOneSuitDeck(3, Spades, true),
          Card.getOneSuitDeck(4, Spades, true),
          Card.getOneSuitDeck(5, Spades, true),
          Card.getOneSuitDeck(6, Spades, true),
          Card.getOneSuitDeck(7, Spades, true),
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

  module Board = SpiderRules.StandardBoard
})

module TwoSuit = GameBase.Create({
  include SpiderRules

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
  module Board = SpiderRules.StandardBoard
})

module FourSuit = GameBase.Create({
  include SpiderRules

  let initiateGame = () => {
    let shuffledDeck =
      Array.concatMany(
        [],
        [
          Card.getOneSuitDeck(0, Spades, true),
          Card.getOneSuitDeck(1, Spades, true),
          Card.getOneSuitDeck(2, Clubs, true),
          Card.getOneSuitDeck(3, Clubs, true),
          Card.getOneSuitDeck(4, Hearts, true),
          Card.getOneSuitDeck(5, Hearts, true),
          Card.getOneSuitDeck(6, Diamonds, true),
          Card.getOneSuitDeck(7, Diamonds, true),
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
  module Board = SpiderRules.StandardBoard
})

module Scorpion = GameBase.Create({
  include SpiderRules

  let initiateGame = () => {
    let shuffledDeck = Card.getDeck(0, true)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal
          ->ArrayAux.popN(7)
          ->Array.mapWithIndex((v, i) => {
            if i >= 3 {
              {...v, hidden: false}
            } else {
              v
            }
          }),
          deckToDeal
          ->ArrayAux.popN(7)
          ->Array.mapWithIndex((v, i) => {
            if i >= 3 {
              {...v, hidden: false}
            } else {
              v
            }
          }),
          deckToDeal
          ->ArrayAux.popN(7)
          ->Array.mapWithIndex((v, i) => {
            if i >= 3 {
              {...v, hidden: false}
            } else {
              v
            }
          }),
          deckToDeal->ArrayAux.popN(7)->Array.map(v => {...v, hidden: false}),
          deckToDeal->ArrayAux.popN(7)->Array.map(v => {...v, hidden: false}),
          deckToDeal->ArrayAux.popN(7)->Array.map(v => {...v, hidden: false}),
          deckToDeal->ArrayAux.popN(7)->Array.map(v => {...v, hidden: false}),
        ]->flipLastUp,
        foundations: [[], [], [], []],
        stock: [deckToDeal->ArrayAux.popN(3)],
      },
    )
  }

  let pileBaseRules = (i): staticSpace => {
    {
      droppedUpon: (game, dragPile) => {
        let noChildren = game.piles->Array.getUnsafe(i)->Array.length == 0
        let dragPileBase = dragPile->Array.getUnsafe(0)

        if noChildren && dragPileBase.card.rank == RK {
          Some({
            ...game,
            piles: game.piles->ArrayAux.update(i, _ => dragPile)->flipLastUp,
          })
        } else {
          None
        }
      },
      autoProgress: Accept,
      onClick: _ => None,
    }
  }

  let pileRules = (_game, pile, card, i, j): movableSpace => {
    let isLast = j == pile->Array.length - 1

    {
      locationAdjustment: {
        x: 0,
        y: j * 20,
        z: j + 1,
      },
      baseSpace: Pile(i),
      dragPile: () => {
        let dragPile = pile->Array.sliceToEnd(~start=j)
        Some(dragPile)
      },
      autoProgress: () => {
        if isLast {
          SendOrAccept([card])
        } else {
          DoNothing
        }
      },
      droppedUpon: (game, dragPile) => {
        let dragPileBase = dragPile->Array.getUnsafe(0)

        if (
          isLast && Card.rankIsAbove(card, dragPileBase) && dragPileBase.card.suit == card.card.suit
        ) {
          Some({
            ...game,
            piles: game.piles
            ->Array.map(stack => {
              stack->ArrayAux.insertAfter(card, dragPile)
            })
            ->flipLastUp,
          })
        } else {
          None
        }
      },
      onClick: _ => None,
      onStateChange: element => {
        if card.hidden {
          Card.hide(element)
        } else {
          Card.show(element)
        }
      },
    }
  }

  let forEachSpace: GameBase.forEachSpace<game, space, dragPile> = (game: game, f) => {
    game.piles->Array.forEachWithIndex((pile, i) => {
      f(Pile(i), pileBaseRules(i)->GameBase.Static)

      pile->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), pileRules(game, pile, card, i, j)->Movable)
      })
    })

    game.foundations->Array.forEachWithIndex((foundation, i) => {
      f(Foundation(i), foundationBaseRules(i)->Static)

      foundation->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), foundationRules(i, j)->Movable)
      })
    })

    game.stock->Array.forEachWithIndex((stockGroup, i) => {
      stockGroup->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), stockGroupRules(game, card, i, j)->Movable)
      })
    })
  }

  module Board = {
    @react.component
    let make = (
      ~setRef,
      ~onMouseDown as _,
      ~setGame as _,
      ~moveToState as _,
      ~autoProgress as _,
      ~game as _,
      ~undo as _,
      ~isWin as _,
      ~onClick as _,
    ) => {
      <React.Fragment>
        <div className="flex flex-row">
          <div
            key={Stock->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Stock))}
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
})
