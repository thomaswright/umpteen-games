open Webapi.Dom
open Common

module GameRules: GameBase.GameRules = {
  @decco
  type space = Card(Card.card) | Foundation | Pile(int) | Waste | Stock

  let getSpace = element => {
    switch element->Element.id->Js.Json.parseExn->space_decode {
    | Ok(d) => Some(d)
    | _ => None
    }
  }

  let spaceToString = space => {
    space->space_encode->Js.Json.stringify
  }

  type dragPile = Card.sides
  @decco
  type deck = array<Card.sides>
  @decco
  type game = {
    piles: array<array<option<Card.sides>>>,
    foundations: array<Card.sides>,
    stock: array<Card.sides>,
    waste: array<Card.sides>,
  }

  type movableSpace = GameBase.movableSpace<game, space, dragPile>
  type staticSpace = GameBase.staticSpace<game, dragPile>

  let applyLiftToDragPile = (dragPile: dragPile, lift) => {
    lift(Card(dragPile.card), 0)
  }

  let applyMoveToDragPile = (dragPile: dragPile, move) => {
    move(Card(dragPile.card), 0, 0)
  }

  let initiateGame = () => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled
    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(1)->Array.map(v => Some(v)),
          deckToDeal->ArrayAux.popN(2)->Array.map(v => Some(v)),
          deckToDeal->ArrayAux.popN(3)->Array.map(v => Some(v)),
          deckToDeal->ArrayAux.popN(4)->Array.map(v => Some(v)),
          deckToDeal->ArrayAux.popN(5)->Array.map(v => Some(v)),
          deckToDeal->ArrayAux.popN(6)->Array.map(v => Some(v)),
          deckToDeal->ArrayAux.popN(7)->Array.map(v => Some(v)),
        ],
        foundations: [],
        stock: deckToDeal.contents,
        waste: [],
      },
    )
  }

  let winCheck = (game: game) => {
    game.piles->Array.every(pile => pile->Array.every(v => v->Option.isNone)) &&
    game.stock->Array.length == 0 &&
    game.waste->Array.length == 0
  }

  let removeDragFromGame = (game: game, dragPile: dragPile) => {
    let removeFromPiles = x =>
      x->Array.map(sCard => {
        switch sCard {
        | Some(sCard) =>
          if sCard == dragPile {
            None
          } else {
            Some(sCard)
          }
        | None => None
        }
      })

    let removeDragPile = x => x->Array.filter(sCard => dragPile != sCard)

    {
      foundations: game.foundations->removeDragPile,
      piles: game.piles->Array.map(removeFromPiles),
      stock: game.stock->removeDragPile,
      waste: game.waste->removeDragPile,
    }
  }

  let pileBaseRules = (i): staticSpace => {
    {
      droppedUpon: (_game, _dragPile) => None,
      autoProgress: DoNothing,
      onClick: _ => None,
    }
  }

  let arePair = (a: Card.sides, b: Card.sides) => {
    switch a.card.rank {
    | RA if b.card.rank == RQ => true
    | R2 if b.card.rank == RJ => true
    | R3 if b.card.rank == R10 => true
    | R4 if b.card.rank == R9 => true
    | R5 if b.card.rank == R8 => true
    | R6 if b.card.rank == R7 => true
    | R7 if b.card.rank == R6 => true
    | R8 if b.card.rank == R5 => true
    | R9 if b.card.rank == R4 => true
    | R10 if b.card.rank == R3 => true
    | RJ if b.card.rank == R2 => true
    | RQ if b.card.rank == RA => true
    | _ => false
    }
  }

  let isExposed = (game, i, j) => {
    i == game.piles->Array.length - 1 ||
      game.piles
      ->Array.get(i + 1)
      ->Option.mapOr(false, pile => {
        let leftEmpty = pile->Array.get(j)->Option.isNone
        let rightEmpty = pile->Array.get(j + 1)->Option.isNone
        leftEmpty && rightEmpty
      })
  }

  let pileRules = (game, pile, card, i, j): movableSpace => {
    {
      locationAdjustment: {
        x: j * 70,
        y: 0,
        z: j + 1,
      },
      baseSpace: Pile(i),
      dragPile: () => {
        if i == game.piles->Array.length - 1 {
          Some(card)
        } else if isExposed(game, i, j) {
          Some(card)
        } else {
          None
        }
      },
      autoProgress: () => {
        DoNothing
        // if isLast {
        //   SendOrAccept(card)
        // } else {
        //   DoNothing
        // }
      },
      droppedUpon: (game, dragPile) => {
        if card.card.suit == Clubs && card.card.rank == R2 {
          Console.log4(game, card.card, isExposed(game, i, j), arePair(dragPile, card))
        }
        if isExposed(game, i, j) && arePair(dragPile, card) {
          Some({
            ...game,
            foundations: Array.concat(game.foundations, [card, dragPile]),
            piles: game.piles->ArrayAux.update(i, stack => {
              stack->ArrayAux.update(j, _ => None)
            }),
          })
        } else {
          None
        }

        // else if card.card.rank == RK {
        //   Some({
        //     ...game,
        //     foundations: Array.concat(game.foundations, [dragPile]),
        //   })
        // }
      },
      onClick: game => {
        if card.card.rank == RK {
          Some({
            ...game,
            foundations: game.foundations->Array.concat([card]),
            piles: game.piles->ArrayAux.update(i, stack => {
              stack->ArrayAux.update(j, _ => None)
            }),
          })
        } else {
          None
        }
      },
      onStateChange: element => Card.showOrHide(card, element),
    }
  }

  let foundationBaseRule = (): staticSpace => {
    {
      autoProgress: DoNothing,
      droppedUpon: (game, dragPile) => {
        None
      },
      onClick: _ => None,
    }
  }

  let foundationRules = (game, card, i): movableSpace => {
    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: i + 1,
      },
      baseSpace: Foundation,
      dragPile: () => {
        None
      },
      autoProgress: () => Seek,
      droppedUpon: (game, dragPile) => {
        None
      },
      onClick: _ => None,
      onStateChange: element => Card.showOrHide(card, element),
    }
  }

  let wasteRules = (game, card, i): movableSpace => {
    baseSpace: Waste,
    locationAdjustment: {
      x: 0,
      y: 0,
      z: i + 1,
    },
    dragPile: () => {
      if i == game.waste->Array.length - 1 {
        Some(card)
      } else {
        None
      }
    },
    autoProgress: () => DoNothing,
    droppedUpon: (game, dragPile) => {
      if i == game.waste->Array.length - 1 && arePair(dragPile, card) {
        Some({
          ...game,
          waste: game.waste->Array.slice(~start=0, ~end=game.waste->Array.length - 1),
          foundations: Array.concat(game.foundations, [card, dragPile]),
        })
      } else {
        None
      }
    },
    onClick: _ => None,
    onStateChange: element => Card.showOrHide(card, element),
  }

  let stockRules = (game, card, i): movableSpace => {
    baseSpace: Stock,
    locationAdjustment: {
      x: 0,
      y: 0,
      z: i + 1,
    },
    dragPile: () => {
      if i == game.stock->Array.length - 1 {
        Some(card)
      } else {
        None
      }
    },
    autoProgress: () => DoNothing,
    droppedUpon: (game, dragPile) => {
      if i == game.stock->Array.length - 1 && arePair(dragPile, card) {
        Some({
          ...game,
          stock: game.stock->Array.slice(~start=0, ~end=game.stock->Array.length - 1),
          foundations: Array.concat(game.foundations, [card, dragPile]),
        })
      } else {
        None
      }
    },
    onClick: game => {
      if card.card.rank == RK {
        Some({
          ...game,
          stock: game.stock->Array.slice(~start=0, ~end=game.stock->Array.length - 1),
          foundations: Array.concat(game.foundations, [card]),
        })
      } else {
        Some({
          ...game,
          stock: game.stock->Array.slice(~start=0, ~end=game.stock->Array.length - 1),
          waste: game.waste->Array.concat(
            game.stock->Array.sliceToEnd(~start=game.stock->Array.length - 1),
          ),
        })
      }
    },
    onStateChange: element => Card.showOrHide(card, element),
  }

  let stockBaseRules = (): staticSpace => {
    autoProgress: DoNothing,
    droppedUpon: (_game, _dragPile) => {
      None
    },
    onClick: game => {
      Some({
        ...game,
        stock: game.waste->Array.toReversed,
        waste: [],
      })
    },
  }

  let forEachSpace = (game: game, f) => {
    game.piles->Array.forEachWithIndex((pile, i) => {
      f(Pile(i), pileBaseRules(i)->GameBase.Static)

      pile->Array.forEachWithIndex((card, j) => {
        card->Option.mapOr(
          (),
          card => f(Card(card.card), pileRules(game, pile, card, i, j)->Movable),
        )
      })
    })

    game.foundations->Array.forEachWithIndex((card, i) => {
      f(Foundation, foundationBaseRule()->Static)

      f(Card(card.card), foundationRules(game, card, i)->Movable)
    })

    game.waste->Array.forEachWithIndex((card, i) => {
      f(Card(card.card), wasteRules(game, card, i)->Movable)
    })

    game.stock->Array.forEachWithIndex((card, i) => {
      f(Card(card.card), stockRules(game, card, i)->Movable)
    })

    f(Stock, stockBaseRules()->Static)
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
        <div className="flex flex-row ">
          <div
            key={Stock->spaceToString}
            id={Stock->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Stock))}
            className=" bg-black opacity-20 rounded w-14 h-20"
          />
          <div
            key={Waste->spaceToString}
            id={Waste->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Waste))}
            className="bg-black opacity-20 w-14 h-20 ml-3"
          />
          <div
            key={Foundation->spaceToString}
            id={Foundation->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation))}
            style={{marginLeft: "280px"}}
            className=" border rounded w-14 h-20"
          />
        </div>
        <div className="flex flex-col gap-3 mt-5">
          {[[], [], [], [], [], [], []]
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Pile(i)->spaceToString}
              id={Pile(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
              style={{
                marginLeft: ((6 - i) * 35)->Int.toString ++ "px",
              }}
              className={" w-14 h-10"}
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

module Game = GameBase.Create(GameRules)
