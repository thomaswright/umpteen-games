open Webapi.Dom
open Common

module GameRules: GameBase.GameRules = {
  @decco
  type space = Card(Card.card) | Foundation(int) | Pile(int)

  let getSpace = element => {
    switch element->Element.id->Js.Json.parseExn->space_decode {
    | Ok(d) => Some(d)
    | _ => None
    }
  }

  let spaceToString = space => {
    space->space_encode->Js.Json.stringify
  }

  type dragPile = array<Card.sides>
  @decco
  type deck = array<Card.sides>
  @decco
  type game = {
    piles: array<array<Card.sides>>,
    foundations: array<array<Card.sides>>,
  }

  type movableSpace = GameBase.movableSpace<game, space, dragPile>
  type staticSpace = GameBase.staticSpace<game, dragPile>

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

  let initiateGame = () => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled
    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(2),
          deckToDeal->ArrayAux.popN(1),
        ],
        foundations: [[], [], [], []],
      },
    )
  }

  let winCheck = (game: game) => {
    game.piles->Array.every(pile => pile->Array.length == 0)
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
    }
  }

  let pileBaseRules = (game, i): staticSpace => {
    {
      droppedUpon: (gameRemoved, dragPile) => {
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
      onClick: _ => None,
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
            piles: game.piles->Array.map(stack => {
              stack->ArrayAux.insertAfter(card, dragPile)
            }),
          })
        } else {
          None
        }
      },
      onClick: _ => None,
      onStateChange: _element => {
        ()
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
          })
        } else {
          None
        }
      },
      onClick: _ => None,
    }
  }

  let foundationRules = (_game, _card, i, j): movableSpace => {
    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: j + 1,
      },
      baseSpace: Foundation(i),
      dragPile: () => None,
      autoProgress: () => Seek,
      droppedUpon: (_game, _dragPile) => None,
      onClick: _ => None,
      onStateChange: _ => (),
    }
  }

  let forEachSpace = (game: game, f) => {
    game.piles->Array.forEachWithIndex((pile, i) => {
      f(Pile(i), pileBaseRules(game, i)->GameBase.Static)

      pile->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), pileRules(pile, card, i, j)->Movable)
      })
    })

    game.foundations->Array.forEachWithIndex((foundation, i) => {
      f(Foundation(i), foundationBaseRules(i)->Static)

      foundation->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), foundationRules(game, card, i, j)->Movable)
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
          {[[], [], [], [], [], [], [], [], [], []]
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
