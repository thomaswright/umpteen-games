open Webapi.Dom
open Common
open GameBase

module GameRules: GameBase.GameRules = {
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

  type dragPile = array<Card.card>
  @decco
  type deck = array<Card.card>

  @decco
  type game = {
    piles: array<array<Card.card>>,
    foundations: array<array<Card.card>>,
    stock: array<array<Card.card>>,
  }

  type movableSpace = GameBase.movableSpace<game, space, dragPile>
  type staticSpace = GameBase.staticSpace<game, dragPile>

  let dragPileValidation = dragPile => {
    let (dragPileIsValid, _) =
      dragPile
      ->Array.toReversed
      ->Array.reduce((true, None), ((isStillValid, onTop), onBottom) => {
        !isStillValid
          ? (false, None)
          : switch (onTop, onBottom) {
            | (Some(onTop), onBottom) => (Card.rankIsBelow(onTop, onBottom), Some(onBottom))
            | _ => (true, Some(onBottom))
            }
      })
    dragPileIsValid
  }

  let initiateGame = () => {
    let shuffledDeck = Array.concatMany(
      [],
      [
        Card.getOneSuitDeck(0, Spades)->Array.toShuffled,
        Card.getOneSuitDeck(1, Spades)->Array.toShuffled,
        Card.getOneSuitDeck(2, Spades)->Array.toShuffled,
        Card.getOneSuitDeck(3, Spades)->Array.toShuffled,
        Card.getOneSuitDeck(4, Spades)->Array.toShuffled,
        Card.getOneSuitDeck(5, Spades)->Array.toShuffled,
        Card.getOneSuitDeck(6, Spades)->Array.toShuffled,
        Card.getOneSuitDeck(7, Spades)->Array.toShuffled,
      ],
    )

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
        ],
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

  let winCheck = (game: game) => {
    game.piles->Array.every(pile => pile->Array.length == 0) &&
      game.stock->Array.every(stockGroup => stockGroup->Array.length == 0)
  }

  let removeDragFromGame = (game: game, dragPile: dragPile) => {
    let removeDragPile = x =>
      x->Array.filter(sCard => {
        !(dragPile->Array.some(dCard => sCard == dCard))
      })

    {
      foundations: game.foundations->Array.map(removeDragPile),
      piles: game.piles->Array.map(removeDragPile),
      stock: game.stock->Array.map(removeDragPile),
    }
  }

  let applyLiftToDragPile = (dragPile, lift) => {
    dragPile->Array.forEachWithIndex((v, j) => {
      lift(Card(v), j)
    })
  }

  let applyMoveToDragPile = (dragPile, move) => {
    dragPile->Array.forEachWithIndex((v, j) => {
      move(Card(v), 0, j * 20)
    })
  }

  let pileBaseRules = (i): staticSpace => {
    {
      droppedUpon: (game, dragPile) => {
        let noChildren = game.piles->Array.getUnsafe(i)->Array.length == 0

        if noChildren {
          Some({
            ...game,
            piles: game.piles->ArrayAux.update(i, _ => dragPile),
          })
        } else {
          None
        }
      },
      autoProgress: Accept,
    }
  }

  let pileRules = (game, pile, card, i, j): movableSpace => {
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
        if dragPile->dragPileValidation {
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
            piles: game.piles->Array.map(stack => {
              stack->ArrayAux.insertAfter(card, dragPile)
            }),
          })
        } else {
          None
        }
      },
      onClick: _ => None,
      onMove: (~hide as _, ~show) => {
        if isLast {
          show()
        } else {
          ()
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
        let valid = dragPile->dragPileValidation
        if fullStack && noChildren && valid {
          Some({
            ...game,
            foundations: game.foundations->ArrayAux.update(i, _ => dragPile),
          })
        } else {
          None
        }
      },
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
      droppedUpon: (game, dragPile) => {
        None
      },
      onClick: _ => None,
      onMove: (~hide as _, ~show) => {show()},
    }
  }

  let stockGroupRules = (game, card, i, j): movableSpace => {
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
      droppedUpon: (game, dragPile) => {
        None
      },
      onClick: game => {
        game.stock
        ->Common.ArrayAux.getLast
        ->Option.map(stockGroup => {
          {
            ...game,
            piles: game.piles->Array.mapWithIndex((pile, i) => {
              Array.concat(pile, [stockGroup->Array.getUnsafe(i)])
            }),
            stock: game.stock->Array.slice(~start=0, ~end=game.stock->Array.length - 1),
          }
        })
      },
      onMove: (~hide, ~show as _) => {
        hide()
      },
    }
  }

  let getRule: GameBase.getRule<game, space, dragPile> = (game: game, match: space) => {
    let result = ref(None)

    game.piles->Array.forEachWithIndex((pile, i) => {
      if Pile(i) == match {
        result := pileBaseRules(i)->Static->Some
      }

      pile->Array.forEachWithIndex((card, j) => {
        if Card(card) == match {
          result := pileRules(game, pile, card, i, j)->Movable->Some
        }
      })
    })

    game.foundations->Array.forEachWithIndex((foundation, i) => {
      if Foundation(i) == match {
        result := foundationBaseRules(i)->Static->Some
      }

      foundation->Array.forEachWithIndex((card, j) => {
        if Card(card) == match {
          result := foundationRules(i, j)->Movable->Some
        }
      })
    })

    game.stock->Array.forEachWithIndex((stockGroup, i) => {
      stockGroup->Array.forEachWithIndex((card, j) => {
        if Card(card) == match {
          result := stockGroupRules(game, card, i, j)->Movable->Some
        }
      })
    })

    result.contents
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
    let make = (~setRef, ~onMouseDown, ~onClick, ~deck) => {
      <React.Fragment>
        {deck
        ->Array.map(card => {
          <Card.Display
            card={card}
            key={Card(card)->spaceToString}
            id={Card(card)->spaceToString}
            onClick={onClick}
            cardRef={ReactDOM.Ref.callbackDomRef(setRef(Card(card)))}
            onMouseDown={onMouseDown}
            hidden={true}
          />
        })
        ->React.array}
      </React.Fragment>
    }
  }
}

module Game = GameBase.Create(GameRules)
