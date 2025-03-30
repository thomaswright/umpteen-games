open Webapi.Dom
open Common
open GameBase

module GameRules: GameBase.GameRules = {
  @decco
  type item = Card(Card.card) | Tarot(Tarot.card)

  @decco
  type space =
    | Item(item)
    | TarotUp
    | TarotDown
    | Foundation(int)
    | Pile(int)
    | Free

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
    foundations: array<array<Card.card>>,
    tarotUp: array<Tarot.card>,
    tarotDown: array<Tarot.card>,
    free: option<item>,
  }

  type movableSpace = GameBase.movableSpace<game, space, dragPile>
  type staticSpace = GameBase.staticSpace<game, dragPile>

  let initiateGame = () => {
    let fullDeck =
      Array.concat(
        Card.getDeck(0)->Array.toShuffled->Array.map(card => Card(card)),
        Tarot.getDeck(0)->Array.toShuffled->Array.map(card => Tarot(card)),
      )->Array.toShuffled

    let deckWithoutAces = fullDeck->Array.filter(card => {
      switch card {
      | Card(card) => card.rank != RA
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
          [{rank: RA, suit: Clubs, deck: 0}],
          [{rank: RA, suit: Diamonds, deck: 0}],
          [{rank: RA, suit: Hearts, deck: 0}],
          [{rank: RA, suit: Spades, deck: 0}],
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
    lift(Item(dragPile), 0)
  }

  let applyMoveToDragPile = (dragPile, move) => {
    move(Item(dragPile), 0, 0)
  }

  let pileBaseRules = (i): staticSpace => {
    {
      droppedUpon: (game, dragPile) => {
        let noChildren = game.piles->Array.getUnsafe(i)->Array.length == 0

        if noChildren {
          Some({
            ...game,
            piles: game.piles->ArrayAux.update(i, _ => [dragPile]),
          })
        } else {
          None
        }
      },
      autoProgress: DoNothing,
    }
  }

  let pileRules = (pile, item, i, j): movableSpace => {
    let isLast = j == pile->Array.length - 1

    {
      locationAdjustment: {
        x: 0,
        y: j * 20,
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
          if isLast && Card.rankIsAdjacent(card, dragCard) && dragCard.suit == card.suit {
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
    }
  }

  let foundationBaseRules = (i): staticSpace => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        let noChildren = game.foundations->Array.getUnsafe(i)->Array.length == 0
        switch dragPile {
        | Card(card) =>
          if noChildren && card.rank == RA {
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
    }
  }

  let foundationRules = (card: Card.card, i, j): movableSpace => {
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
          if dragCard.suit == card.suit && Card.rankIsBelow(card, dragCard) {
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
    }
  }

  let tarotUpBaseRules = () => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        let noChildren = game.tarotUp->Array.length == 0
        switch dragPile {
        | Tarot(tarot) =>
          if noChildren && tarot.rank == R0 {
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
    }
  }

  let tarotUpRules = (tarot: Tarot.card, j): movableSpace => {
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
          if Tarot.rankIsBelow(tarot, dragTarot) {
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
    }
  }

  let tarotDownBaseRules = () => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        let noChildren = game.tarotDown->Array.length == 0
        switch dragPile {
        | Tarot(tarot) =>
          if noChildren && tarot.rank == R21 {
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
    }
  }

  let tarotDownRules = (tarot: Tarot.card, j): movableSpace => {
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
    }
  }

  let getRule: GameBase.getRule<game, space, dragPile> = (game: game, match: space) => {
    let result = ref(None)

    game.piles->Array.forEachWithIndex((pile, i) => {
      if Pile(i) == match {
        result := pileBaseRules(i)->Static->Some
      }

      pile->Array.forEachWithIndex((card, j) => {
        if Item(card) == match {
          result := pileRules(pile, card, i, j)->Movable->Some
        }
      })
    })

    game.foundations->Array.forEachWithIndex((foundation, i) => {
      if Foundation(i) == match {
        result := foundationBaseRules(i)->Static->Some
      }

      foundation->Array.forEachWithIndex((card, j) => {
        if Item(Card(card)) == match {
          result := foundationRules(card, i, j)->Movable->Some
        }
      })
    })

    if TarotUp == match {
      result := tarotUpBaseRules()->Static->Some
    }

    game.tarotUp->Array.forEachWithIndex((card, i) => {
      if Item(Tarot(card)) == match {
        result := tarotUpRules(card, i)->Movable->Some
      }
    })

    if TarotDown == match {
      result := tarotDownBaseRules()->Static->Some
    }

    game.tarotDown->Array.forEachWithIndex((card, i) => {
      if Item(Tarot(card)) == match {
        result := tarotDownRules(card, i)->Movable->Some
      }
    })

    if Free == match {
      result := freeBaseRules()->Static->Some
    }

    switch game.free {
    | Some(free) =>
      if Item(free) == match {
        result := freeRules(free)->Movable->Some
      }
    | None => ()
    }

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
        <div className="flex flex-row  ">
          <div
            className="flex flex-row justify-between"
            style={{
              width: "290px",
            }}>
            <div
              key={TarotUp->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(TarotUp))}
              className=" border border-slate-300 bg-slate-200 rounded w-14 h-20 flex 
              flex-row items-center justify-center text-xl font-bold text-slate-400">
              {"0"->React.string}
            </div>
            <div
              key={TarotDown->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(TarotDown))}
              className=" border border-slate-300 bg-slate-200 rounded w-14 h-20 flex 
              flex-row items-center justify-center text-xl font-bold text-slate-400">
              {"21"->React.string}
            </div>
          </div>
          <div
            key={Free->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Free))}
            className="outline outline-2 outline-purple-300 bg-purple-100 rounded w-14 h-20 mx-10"
          />
          <div className="flex flex-row gap-3">
            {[[], [], [], []]
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Foundation(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
                className=" border  border-slate-200 bg-slate-100 rounded w-14 h-20"
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
              className=" border border-slate-200 bg-slate-100  rounded w-14 h-20"
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
              key={Item(item)->spaceToString}
              id={Item(item)->spaceToString}
              cardRef={ReactDOM.Ref.callbackDomRef(setRef(Item(item)))}
              onMouseDown={onMouseDown}
            />
          | Tarot(tarot) =>
            <Tarot.Display
              card={tarot}
              key={Item(item)->spaceToString}
              id={Item(item)->spaceToString}
              cardRef={ReactDOM.Ref.callbackDomRef(setRef(Item(item)))}
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
