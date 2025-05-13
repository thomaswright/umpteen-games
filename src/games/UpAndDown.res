open Webapi.Dom
open Common
open GameBase

module GameRules: GameBase.GameRules = {
  @decco
  type item = Card(Card.sides) | Tarot(Tarot.sides)

  @decco
  type spaceItem = SpaceCard(Card.card) | SpaceTarot(Tarot.card)

  @decco
  type space =
    | Item(spaceItem)
    | TarotUp
    | TarotDown
    | Foundation(int)
    | Pile(int)
    | Free

  let itemToSpaceItem = space => {
    switch space {
    | Card(v) => SpaceCard(v.card)
    | Tarot(v) => SpaceTarot(v.card)
    }
  }

  let getSpace = element => {
    switch element->Element.id->Js.Json.parseExn->space_decode {
    | Ok(d) => Some(d)
    | _ => None
    }
  }

  type dragPile = item

  let spaceToString = space => {
    space->space_encode->Js.Json.stringify
  }

  @decco
  type deck = array<item>

  @decco
  type game = {
    piles: array<array<item>>,
    foundations: array<array<Card.sides>>,
    tarotUp: array<Tarot.sides>,
    tarotDown: array<Tarot.sides>,
    free: option<item>,
  }

  type movableSpace = GameBase.movableSpace<game, space, dragPile>
  type staticSpace = GameBase.staticSpace<game, dragPile>

  let initiateGame = () => {
    let fullDeck =
      Array.concat(
        Card.getDeck(0, false)->Array.toShuffled->Array.map(v => Card(v)),
        Tarot.getDeck(0, false)->Array.toShuffled->Array.map(v => Tarot(v)),
      )->Array.toShuffled

    let deckWithoutAces = fullDeck->Array.filter(card => {
      switch card {
      | Card(card) => card.card.rank != RA
      | _ => true
      }
    })

    let deckToDeal = ref(deckWithoutAces)

    (
      fullDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          [],
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
        ],
        foundations: [
          [{card: {rank: RA, suit: Clubs, deck: 0}, hidden: false}],
          [{card: {rank: RA, suit: Diamonds, deck: 0}, hidden: false}],
          [{card: {rank: RA, suit: Hearts, deck: 0}, hidden: false}],
          [{card: {rank: RA, suit: Spades, deck: 0}, hidden: false}],
        ],
        tarotUp: [],
        tarotDown: [],
        free: None,
      },
    )
  }

  let winCheck = (game: game) => {
    game.piles->Array.every(pile => pile->Array.length == 0) && game.free->Option.isNone
  }

  let removeDragFromGame = (game: game, dragPile: dragPile): game => {
    {
      ...game,
      foundations: game.foundations->Array.map(x =>
        x->Array.filter(sCard => {
          Card(sCard) != dragPile
        })
      ),
      piles: game.piles->Array.map(x =>
        x->Array.filter(sCard => {
          sCard != dragPile
        })
      ),
      free: game.free->Option.flatMap(card => card != dragPile ? Some(card) : None),
    }
  }

  let applyLiftToDragPile = (dragPile, lift) => {
    lift(Item(dragPile->itemToSpaceItem), 0)
  }

  let applyMoveToDragPile = (dragPile, move) => {
    move(Item(dragPile->itemToSpaceItem), 0, 0)
  }

  let pileBaseRules = (game, i): staticSpace => {
    {
      droppedUpon: (gameRemoved, dragPile) => {
        let noChildren = game.piles->Array.getUnsafe(i)->Array.length == 0

        if noChildren {
          Some({
            ...gameRemoved,
            piles: gameRemoved.piles->ArrayAux.update(i, _ => [dragPile]),
          })
        } else {
          None
        }
      },
      autoProgress: DoNothing,
      onClick: _ => None,
    }
  }

  let pileRules = (pile, item, i, j): movableSpace => {
    let isLast = j == pile->Array.length - 1

    {
      locationAdjustment: {
        x: 0,
        y: j * Common.space,
        z: j + 1,
      },
      baseSpace: Pile(i),
      dragPile: () => {
        if isLast {
          Some(item)
        } else {
          None
        }
      },
      autoProgress: () => {
        if isLast {
          SendOrAccept(item)
        } else {
          DoNothing
        }
      },
      droppedUpon: (game, dragPile) => {
        switch (dragPile, item) {
        | (Card(dragCard), Card(card)) =>
          if isLast && Card.rankIsAdjacent(card, dragCard) && dragCard.card.suit == card.card.suit {
            Some({
              ...game,
              piles: game.piles->Array.map(stack => {
                stack->ArrayAux.insertAfter(item, [dragPile])
              }),
            })
          } else {
            None
          }
        | (Tarot(dragCard), Tarot(card)) =>
          if isLast && Tarot.rankIsAdjacent(card, dragCard) {
            Some({
              ...game,
              piles: game.piles->Array.map(stack => {
                stack->ArrayAux.insertAfter(item, [dragPile])
              }),
            })
          } else {
            None
          }
        | _ => None
        }
      },
      onClick: _ => None,
      onStateChange: _ => (),
    }
  }

  let foundationBaseRules = (i): staticSpace => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        let noChildren = game.foundations->Array.getUnsafe(i)->Array.length == 0
        switch dragPile {
        | Card(card) =>
          if noChildren && card.card.rank == RA {
            Some({
              ...game,
              foundations: game.foundations->ArrayAux.update(i, _ => [card]),
            })
          } else {
            None
          }
        | _ => None
        }
      },
      onClick: _ => None,
    }
  }

  let foundationRules = (card: Card.sides, pile, i, j): movableSpace => {
    let isLast = j == pile->Array.length - 1

    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: j + 1,
      },
      baseSpace: Foundation(i),
      dragPile: () => None,
      autoProgress: () => Seek,
      droppedUpon: (game, dragPile) => {
        switch dragPile {
        | Card(dragCard) =>
          if isLast && dragCard.card.suit == card.card.suit && Card.rankIsAbove(dragCard, card) {
            Some({
              ...game,
              foundations: game.foundations->Array.map(stack => {
                stack->ArrayAux.insertAfter(card, [dragCard])
              }),
            })
          } else {
            None
          }
        | _ => None
        }
      },
      onClick: _ => None,
      onStateChange: _ => (),
    }
  }

  let tarotUpBaseRules = () => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        let noChildren = game.tarotUp->Array.length == 0
        switch dragPile {
        | Tarot(tarot) =>
          if noChildren && tarot.card.rank == R0 {
            Some({
              ...game,
              tarotUp: [tarot],
            })
          } else {
            None
          }
        | _ => None
        }
      },
      onClick: _ => None,
    }
  }

  let tarotUpRules = (tarot: Tarot.sides, j): movableSpace => {
    {
      locationAdjustment: {
        x: 10 * j,
        y: 0,
        z: j,
      },
      baseSpace: TarotUp,
      dragPile: () => None,
      autoProgress: () => Seek,
      droppedUpon: (game, dragPile) => {
        switch dragPile {
        | Tarot(dragTarot) =>
          if Tarot.rankIsAbove(dragTarot, tarot) {
            Some({
              ...game,
              tarotUp: Array.concat(game.tarotUp, [dragTarot]),
            })
          } else {
            None
          }
        | _ => None
        }
      },
      onClick: _ => None,
      onStateChange: _ => (),
    }
  }

  let tarotDownBaseRules = () => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        let noChildren = game.tarotDown->Array.length == 0
        switch dragPile {
        | Tarot(tarot) =>
          if noChildren && tarot.card.rank == R21 {
            Some({
              ...game,
              tarotDown: [tarot],
            })
          } else {
            None
          }
        | _ => None
        }
      },
      onClick: _ => None,
    }
  }

  let tarotDownRules = (tarot: Tarot.sides, j): movableSpace => {
    {
      locationAdjustment: {
        x: -10 * j,
        y: 0,
        z: j,
      },
      baseSpace: TarotDown,
      dragPile: () => None,
      autoProgress: () => Seek,
      droppedUpon: (game, dragPile) => {
        switch dragPile {
        | Tarot(dragTarot) =>
          if Tarot.rankIsAbove(tarot, dragTarot) {
            Some({
              ...game,
              tarotDown: Array.concat(game.tarotDown, [dragTarot]),
            })
          } else {
            None
          }
        | _ => None
        }
      },
      onClick: _ => None,
      onStateChange: _ => (),
    }
  }

  let freeBaseRules = (): staticSpace => {
    autoProgress: DoNothing,
    droppedUpon: (game, dragPile) => {
      switch game.free {
      | Some(_) => None
      | None => Some({...game, free: Some(dragPile)})
      }
    },
    onClick: _ => None,
  }

  let freeRules = (card): movableSpace => {
    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: 1,
      },
      baseSpace: Free,
      autoProgress: () => Send(card),
      dragPile: () => Some(card),
      droppedUpon: (_game, _dragPile) => None,
      onClick: _ => None,
      onStateChange: _ => (),
    }
  }

  let forEachSpace: GameBase.forEachSpace<game, space, dragPile> = (game: game, f) => {
    game.piles->Array.forEachWithIndex((pile, i) => {
      f(Pile(i), pileBaseRules(game, i)->Static)

      pile->Array.forEachWithIndex((card, j) => {
        f(Item(card->itemToSpaceItem), pileRules(pile, card, i, j)->Movable)
      })
    })

    game.foundations->Array.forEachWithIndex((foundation, i) => {
      f(Foundation(i), foundationBaseRules(i)->Static)

      foundation->Array.forEachWithIndex((card, j) => {
        f(Item(SpaceCard(card.card)), foundationRules(card, foundation, i, j)->Movable)
      })
    })

    f(TarotUp, tarotUpBaseRules()->Static)

    game.tarotUp->Array.forEachWithIndex((card, i) => {
      f(Item(SpaceTarot(card.card)), tarotUpRules(card, i)->Movable)
    })

    f(TarotDown, tarotDownBaseRules()->Static)

    game.tarotDown->Array.forEachWithIndex((card, i) => {
      f(Item(SpaceTarot(card.card)), tarotDownRules(card, i)->Movable)
    })

    f(Free, freeBaseRules()->Static)

    switch game.free {
    | Some(free) => f(Item(free->itemToSpaceItem), freeRules(free)->Movable)
    | None => ()
    }
  }

  module Board = {
    @react.component
    let make = (~setRef, ~initialGame as _) => {
      <React.Fragment>
        <div className="flex flex-row  ">
          <div
            className="flex flex-row justify-between"
            style={{
              width: "290px",
            }}>
            <div
              key={TarotUp->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(TarotUp))}
              className=" bg-white opacity-10 rounded card-dims flex 
              flex-row items-center justify-center text-xl font-bold text-black">
              {"0"->React.string}
            </div>
            <div
              key={TarotDown->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(TarotDown))}
              className=" bg-white opacity-10  rounded card-dims flex 
              flex-row items-center justify-center text-xl font-bold text-black">
              {"21"->React.string}
            </div>
          </div>
          <div
            key={Free->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Free))}
            className="outline outline-4 outline-black bg-black bg-opacity-20 rounded card-dims mx-10"
          />
          <div className="flex flex-row gap-3">
            {[[], [], [], []]
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Foundation(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
                className="  bg-white opacity-10 rounded card-dims"
              />
            })
            ->React.array}
          </div>
        </div>
        <div className="flex flex-row gap-3 mt-5">
          {[[], [], [], [], [], [], [], [], [], [], []]
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Pile(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
              className=" bg-black opacity-20   rounded card-dims"
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
        ->Array.map(item => {
          switch item {
          | Card(card) =>
            <Card.Display
              multiColor={true}
              card={card}
              key={Item(item->itemToSpaceItem)->spaceToString}
              id={Item(item->itemToSpaceItem)->spaceToString}
              cardRef={ReactDOM.Ref.callbackDomRef(setRef(Item(item->itemToSpaceItem)))}
              onMouseDown={onMouseDown}
            />
          | Tarot(tarot) =>
            <Tarot.Display
              card={tarot}
              key={Item(item->itemToSpaceItem)->spaceToString}
              id={Item(item->itemToSpaceItem)->spaceToString}
              cardRef={ReactDOM.Ref.callbackDomRef(setRef(Item(item->itemToSpaceItem)))}
              onMouseDown={onMouseDown}
            />
          }
        })
        ->React.array}
      </React.Fragment>
    }
  }
}

module Game = GameBase.Create(GameRules)
