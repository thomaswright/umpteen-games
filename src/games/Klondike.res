open Webapi.Dom
open Common
open GameBase

module GameRules: GameBase.GameRules = {
  @decco
  type space = Card(Card.card) | Foundation(int) | Pile(int) | Waste | Stock

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
    stock: array<Card.card>,
    waste: array<Card.card>,
  }

  type movableSpace = GameBase.movableSpace<game, space, dragPile>
  type staticSpace = GameBase.staticSpace<game, dragPile>

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

  let dragPileValidation = dragPile => {
    let (dragPileIsValid, _) =
      dragPile
      ->Array.toReversed
      ->Array.reduce((true, None), ((isStillValid, onTop), onBottom) => {
        !isStillValid
          ? (false, None)
          : switch (onTop, onBottom) {
            | (Some(onTop), onBottom) => (
                Card.rankIsBelow(onTop, onBottom) && onTop->Card.color != onBottom->Card.color,
                Some(onBottom),
              )
            | _ => (true, Some(onBottom))
            }
      })
    dragPileIsValid
  }

  let initiateGame = () => {
    let shuffledDeck = Card.getDeck(0)->Array.toShuffled
    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(2),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(7),
        ],
        foundations: [[], [], [], []],
        stock: deckToDeal.contents,
        waste: [],
      },
    )
  }

  let winCheck = (game: game) => {
    game.piles->Array.every(pile => pile->Array.length == 0) &&
    game.stock->Array.length == 0 &&
    game.waste->Array.length == 0
  }

  let removeDragFromGame = (game: game, dragPile: dragPile) => {
    let removeDragPile = x =>
      x->Array.filter(sCard => {
        !(dragPile->Array.some(dCard => sCard == dCard))
      })

    {
      foundations: game.foundations->Array.map(removeDragPile),
      piles: game.piles->Array.map(removeDragPile),
      stock: game.stock->removeDragPile,
      waste: game.waste->removeDragPile,
    }
  }

  let pileBaseRules = (i): staticSpace => {
    {
      droppedUpon: (game, dragPile) => {
        let dragPileBase = dragPile->Array.getUnsafe(0)
        let noChildren = game.piles->Array.getUnsafe(i)->Array.length == 0

        if noChildren && dragPileBase.rank == RK {
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

  let pileRules = (pile, card, i, j): movableSpace => {
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

        if (
          isLast &&
          Card.rankIsAbove(card, dragPileBase) &&
          dragPileBase->Card.color != card->Card.color
        ) {
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
    }
  }

  let foundationBaseRules = (i): staticSpace => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        let justOne = dragPile->Array.length == 1
        let dragPileBase = dragPile->Array.getUnsafe(0)
        let noChildren = game.foundations->Array.getUnsafe(i)->Array.length == 0

        if noChildren && justOne && dragPileBase.rank == RA {
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

  let foundationRules = (game, card, i, j): movableSpace => {
    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: j + 1,
      },
      baseSpace: Foundation(i),
      dragPile: () => {
        if j == game.foundations->Array.length - 1 {
          Some([card])
        } else {
          None
        }
      },
      autoProgress: () => Seek,
      droppedUpon: (game, dragPile) => {
        let justOne = dragPile->Array.length == 1
        let dragPileBase = dragPile->Array.getUnsafe(0)

        if justOne && dragPileBase.suit == card.suit && Card.rankIsBelow(card, dragPileBase) {
          Some({
            ...game,
            foundations: game.foundations->Array.map(stack => {
              stack->ArrayAux.insertAfter(card, dragPile)
            }),
          })
        } else {
          None
        }
      },
    }
  }

  let wasteRules = (game, card, i) => {
    baseSpace: Waste,
    locationAdjustment: {
      x: 20 * i,
      y: 0,
      z: i + 1,
    },
    dragPile: () => {
      if i == game.waste->Array.length - 1 {
        Some([card])
      } else {
        None
      }
    },
    autoProgress: () => DoNothing,
    droppedUpon: (_, _) => None,
  }

  let stockRules = i => {
    baseSpace: Stock,
    locationAdjustment: {
      x: 0,
      y: 0,
      z: i + 1,
    },
    dragPile: () => None,
    autoProgress: () => DoNothing,
    droppedUpon: (_, _) => None,
  }

  let getRule: GameBase.getRule<game, space, dragPile> = (game: game, match: space) => {
    let result = ref(None)

    game.piles->Array.forEachWithIndex((pile, i) => {
      if Pile(i) == match {
        result := pileBaseRules(i)->Static->Some
      }

      pile->Array.forEachWithIndex((card, j) => {
        if Card(card) == match {
          result := pileRules(pile, card, i, j)->Movable->Some
        }
      })
    })

    game.foundations->Array.forEachWithIndex((foundation, i) => {
      if Foundation(i) == match {
        result := foundationBaseRules(i)->Static->Some
      }

      foundation->Array.forEachWithIndex((card, j) => {
        if Card(card) == match {
          result := foundationRules(game, card, i, j)->Movable->Some
        }
      })
    })

    game.waste->Array.forEachWithIndex((card, i) => {
      if Card(card) == match {
        result := wasteRules(game, card, i)->Movable->Some
      }
    })

    game.stock->Array.forEachWithIndex((card, i) => {
      if Card(card) == match {
        result := stockRules(i)->Movable->Some
      }
    })

    result.contents
  }

  module Custom = {
    let dealToWaste = async (setGame, moveToState, autoProgress) => {
      let f = _ => {
        setGame(game => {
          ...game,
          stock: game.stock->Array.slice(~start=0, ~end=game.stock->Array.length - 1),
          waste: game.waste->Array.concat(
            game.stock->Array.sliceToEnd(~start=game.stock->Array.length - 1),
          ),
        })
        moveToState()
      }

      await numInterval(f, 400, 3)
      autoProgress()
    }

    let restock = (setGame, moveToState) => {
      setGame(game => {
        ...game,
        stock: game.waste->Array.toReversed,
        waste: [],
      })
      moveToState()
    }
  }

  module Board = {
    @react.component
    let make = (
      ~setRef,
      ~onMouseDown as _,
      ~setGame,
      ~moveToState,
      ~autoProgress,
      ~game,
      ~undo as _,
      ~isWin as _,
    ) => {
      <React.Fragment>
        <div className="flex flex-row gap-3">
          <div
            key={Stock->spaceToString}
            id={Stock->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Stock))}
            onClick={_ => {
              if game.stock->Array.length == 0 {
                Custom.restock(setGame, moveToState)
              } else {
                Custom.dealToWaste(setGame, moveToState, autoProgress)->ignore
              }
            }}
            className=" bg-blue-200 rounded w-14 h-20"
            style={{
              zIndex: "53",
            }}
          />
          <div
            key={Waste->spaceToString}
            id={Waste->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Waste))}
            className=" w-14 h-20"
          />
        </div>
        <div className="flex flex-row gap-3 mt-5">
          {[[], [], [], []]
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Foundation(i)->spaceToString}
              id={Foundation(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
              className=" bg-white opacity-10 rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
        <div className="flex flex-row gap-3 mt-5">
          {[[], [], [], [], [], [], []]
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Pile(i)->spaceToString}
              id={Pile(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
              className=" bg-black opacity-20  rounded w-14 h-20"
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
            key={Card(card)->spaceToString}
            id={Card(card)->spaceToString}
            cardRef={ReactDOM.Ref.callbackDomRef(setRef(Card(card)))}
            onMouseDown={onMouseDown}
          />
        })
        ->React.array}
      </React.Fragment>
    }
  }
}

module Game = GameBase.Create(GameRules)
