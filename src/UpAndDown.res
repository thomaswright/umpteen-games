open Webapi.Dom
open Types
open Common

module GameRules = {
  let foundationOffset = 70 * 6

  module Item = {}

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

  let spaceToString = space => {
    space->space_encode->Js.Json.stringify
  }

  let fullDeck =
    Array.concat(
      Card.getShuffledDeck()->Array.map(card => Card(card)),
      Tarot.getShuffledDeck()->Array.map(card => Tarot(card)),
    )->Array.toShuffled

  let deckToDeal = fullDeck->Array.filter(card => {
    switch card {
    | Card(card) => card.rank != RA
    | _ => true
    }
  })

  type game = {
    piles: array<array<item>>,
    foundations: array<array<Card.card>>,
    tarotUp: array<Tarot.card>,
    tarotDown: array<Tarot.card>,
    free: option<item>,
    gameEnded: bool,
  }

  let initiateGame = () => {
    {
      piles: [
        deckToDeal->Array.slice(~start=0, ~end=7),
        deckToDeal->Array.slice(~start=7, ~end=14),
        deckToDeal->Array.slice(~start=14, ~end=21),
        deckToDeal->Array.slice(~start=21, ~end=28),
        deckToDeal->Array.slice(~start=28, ~end=35),
        [],
        deckToDeal->Array.slice(~start=35, ~end=42),
        deckToDeal->Array.slice(~start=42, ~end=49),
        deckToDeal->Array.slice(~start=49, ~end=56),
        deckToDeal->Array.slice(~start=56, ~end=63),
        deckToDeal->Array.slice(~start=63, ~end=70),
      ],
      foundations: [
        [{rank: RA, suit: Clubs}],
        [{rank: RA, suit: Diamonds}],
        [{rank: RA, suit: Hearts}],
        [{rank: RA, suit: Spades}],
      ],
      tarotUp: [],
      tarotDown: [],
      free: None,
      gameEnded: false,
    }
  }

  let getSpaceLocs = (game: game) => {
    // Todo: maybe change to a map
    let cards = ref([])
    let addToCards = card => cards := Array.concat(cards.contents, [card])
    game.piles->ArrayAux.forEach2((_, item, i, j) => {
      addToCards((
        Item(item),
        {
          x: i * 70,
          y: 100 + j * 20,
          z: j + 1,
        },
      ))
    })

    game.foundations->ArrayAux.forEach2((_, card, i, j) => {
      addToCards((
        Item(Card(card)),
        {
          x: foundationOffset + 30 + i * 70,
          y: 0,
          z: j + 1,
        },
      ))
    })

    game.free->Option.mapOr((), item => {
      addToCards((
        Item(item),
        {
          x: foundationOffset - 70,
          y: 0,
          z: 1,
        },
      ))
    })
    game.tarotUp->Array.forEachWithIndex((tarot, i) => {
      addToCards((
        Item(Tarot(tarot)),
        {
          x: 10 * i,
          y: 0,
          z: i,
        },
      ))
    })

    game.tarotDown->Array.forEachWithIndex((tarot, i) => {
      addToCards((
        Item(Tarot(tarot)),
        {
          x: foundationOffset - 30 - 70 * 2 - 10 * i,
          y: 0,
          z: i,
        },
      ))
    })

    cards.contents
  }

  let baseSpace = (dropItem: item, game: game) => {
    let base = ref(None)

    game.piles->ArrayAux.forEach2((_, item, i, _) => {
      if item == dropItem {
        base := Some(Pile(i))
      }
    })

    game.foundations->ArrayAux.forEach2((_, card, i, _) => {
      if Card(card) == dropItem {
        base := Some(Foundation(i))
      }
    })

    game.free->Option.mapOr((), card => {
      if card == dropItem {
        base := Some(Free)
      }
    })

    game.tarotUp->Array.forEachWithIndex((tarot, i) => {
      if Tarot(tarot) == dropItem {
        base := Some(TarotUp)
      }
    })

    game.tarotDown->Array.forEachWithIndex((tarot, i) => {
      if Tarot(tarot) == dropItem {
        base := Some(TarotDown)
      }
    })

    base.contents
  }

  let canDrag = (space, game) => {
    switch space {
    | Item(item) =>
      // check if on top
      switch baseSpace(item, game) {
      | Some(Pile(pileIndex)) =>
        game.piles
        ->Array.get(pileIndex)
        ->Option.flatMap(pile => {
          pile->ArrayAux.getLast
        })
        ->Option.mapOr(false, pileLast => {
          pileLast == item
        })
      | Some(Free) => true
      | _ => false
      }
    | _ => false
    }
  }

  let canDrop = (dragSpace: space, dropSpace: space, game: game) => {
    switch dragSpace {
    | Item(dragItem) => {
        let notDragSpace = dragSpace != dropSpace

        let canBeParent = switch dropSpace {
        | Item(dropItem) =>
          switch baseSpace(dropItem, game) {
          | Some(Free) => false
          | Some(Pile(i)) =>
            let topItem = game.piles->Array.getUnsafe(i)->ArrayAux.getLast
            if topItem == Some(dropItem) {
              switch (dropItem, dragItem) {
              | (Card(c1), Card(c2)) => Card.rankIsBelow(c1, c2) || Card.rankIsAbove(c1, c2)
              | (Tarot(c1), Tarot(c2)) => Tarot.rankIsBelow(c1, c2) || Tarot.rankIsAbove(c1, c2)
              | _ => false
              }
            } else {
              false
            }
          | Some(_) =>
            switch (dropItem, dragItem) {
            | (Card(c1), Card(c2)) => Card.rankIsBelow(c1, c2) || Card.rankIsAbove(c1, c2)
            | (Tarot(c1), Tarot(c2)) => Tarot.rankIsBelow(c1, c2) || Tarot.rankIsAbove(c1, c2)
            | _ => false
            }
          | _ => false
          }
        | TarotUp => dragItem == Tarot({rank: R1})
        | TarotDown => dragItem == Tarot({rank: R21})
        | Pile(i) => game.piles->Array.getUnsafe(i)->Array.length == 0
        | Free => game.free->Option.isNone
        | Foundation(_) => false // Always starts with an Ace
        }

        notDragSpace && canBeParent
      }
    | _ => false
    }
  }

  let onDrop = (dropOnSpace, dragSpace, game, setGame) => {
    switch dragSpace {
    | Item(Card(dragCard)) => {
        let removeDrag = x =>
          x->Array.filter(sCard => {
            sCard != Card(dragCard)
          })

        setGame(game => {
          {
            ...game,
            piles: game.piles->Array.map(removeDrag),
            free: switch game.free {
            | None => None
            | Some(x) => x == Card(dragCard) ? None : Some(x)
            },
          }
        })

        switch dropOnSpace {
        | Item(Card(card)) =>
          setGame(game => {
            {
              ...game,
              foundations: game.foundations->Array.map(stack => {
                stack->ArrayAux.insertAfter(card, [dragCard])
              }),
              piles: game.piles->Array.map(stack => {
                stack->ArrayAux.insertAfter(Card(card), [Card(dragCard)])
              }),
            }
          })

        | Foundation(i) =>
          setGame(game => {
            {
              ...game,
              foundations: game.foundations->ArrayAux.update(i, _ => [dragCard]),
            }
          })
        | Pile(i) =>
          setGame(game => {
            {
              ...game,
              piles: game.piles->ArrayAux.update(i, _ => [Card(dragCard)]),
            }
          })
        | Free =>
          setGame(game => {
            {
              ...game,
              free: Some(Card(dragCard)),
            }
          })
        | _ => ()
        }
      }

    | Item(Tarot(dragTarot)) => {
        let removeDrag = x =>
          x->Array.filter(sCard => {
            sCard != Tarot(dragTarot)
          })

        setGame(game => {
          {
            ...game,
            piles: game.piles->Array.map(removeDrag),
            free: switch game.free {
            | None => None
            | Some(x) => x == Tarot(dragTarot) ? None : Some(x)
            },
          }
        })

        switch dropOnSpace {
        | Item(Tarot(tarot)) =>
          setGame(game => {
            {
              ...game,
              piles: game.piles->Array.map(stack => {
                stack->ArrayAux.insertAfter(Tarot(tarot), [Tarot(dragTarot)])
              }),
              tarotUp: game.tarotUp->ArrayAux.insertAfter(tarot, [dragTarot]),
              tarotDown: game.tarotDown->ArrayAux.insertAfter(tarot, [dragTarot]),
            }
          })

        | Pile(i) =>
          setGame(game => {
            {
              ...game,
              piles: game.piles->ArrayAux.update(i, _ => [Tarot(dragTarot)]),
            }
          })
        | Free =>
          setGame(game => {
            {
              ...game,
              free: Some(Tarot(dragTarot)),
            }
          })
        | _ => ()
        }
      }

    | _ => ()
    }
  }

  let applyMoveToOthers = (space: space, game, move) => {
    ()
  }

  let autoProgress = setGame => {
    let newGame = ref(None)

    setGame(game => {
      // Foundations Check
      game.foundations->Array.forEachWithIndex((foundation, i) => {
        let canMove = (c: Card.card) => {
          switch foundation->ArrayAux.getLast {
          | None => c.rank == RA
          | Some(foundationCard) =>
            Card.rankIsBelow(foundationCard, c) && foundationCard.suit == c.suit
          }
        }
        game.free->Option.mapOr(
          (),
          freeItem => {
            switch freeItem {
            | Card(freeCard) =>
              if newGame.contents->Option.isNone && canMove(freeCard) {
                newGame :=
                  Some({
                    ...game,
                    foundations: game.foundations->ArrayAux.update(
                      i,
                      f => f->Array.concat([freeCard]),
                    ),
                    free: None,
                  })
              }

            | _ => ()
            }
          },
        )

        game.piles->Array.forEachWithIndex(
          (pile, j) => {
            switch pile->ArrayAux.getLast {
            | Some(Card(pileCard)) =>
              if newGame.contents->Option.isNone && canMove(pileCard) {
                newGame :=
                  Some({
                    ...game,
                    foundations: game.foundations->ArrayAux.update(
                      i,
                      f => f->Array.concat([pileCard]),
                    ),
                    piles: game.piles->ArrayAux.update(j, p => p->ArrayAux.removeLast),
                  })
              }
            | _ => ()
            }
          },
        )
      })

      // Tarot Check

      let canMoveTarotUp = (tarotCard: Tarot.card) => {
        switch game.tarotUp->ArrayAux.getLast {
        | None => tarotCard.rank == R0
        | Some(tarotUpCard) => Tarot.rankIsAbove(tarotCard, tarotUpCard)
        }
      }
      let canMoveTarotDown = (tarotCard: Tarot.card) => {
        switch game.tarotUp->ArrayAux.getLast {
        | None => tarotCard.rank == R21
        | Some(tarotUpCard) => Tarot.rankIsBelow(tarotCard, tarotUpCard)
        }
      }

      game.free->Option.mapOr((), freeItem => {
        switch freeItem {
        | Tarot(freeTarot) =>
          if newGame.contents->Option.isNone && canMoveTarotUp(freeTarot) {
            newGame :=
              Some({
                ...game,
                tarotUp: game.tarotUp->Array.concat([freeTarot]),
                free: None,
              })
          }

          if newGame.contents->Option.isNone && canMoveTarotDown(freeTarot) {
            newGame :=
              Some({
                ...game,
                tarotDown: game.tarotDown->Array.concat([freeTarot]),
                free: None,
              })
          }

        | _ => ()
        }
      })

      game.piles->Array.forEachWithIndex((pile, j) => {
        switch pile->ArrayAux.getLast {
        | Some(Tarot(pileTarot)) =>
          if newGame.contents->Option.isNone && canMoveTarotUp(pileTarot) {
            newGame :=
              Some({
                ...game,
                tarotUp: game.tarotUp->Array.concat([pileTarot]),
                piles: game.piles->ArrayAux.update(j, p => p->ArrayAux.removeLast),
              })
          }

          if newGame.contents->Option.isNone && canMoveTarotDown(pileTarot) {
            newGame :=
              Some({
                ...game,
                tarotDown: game.tarotUp->Array.concat([pileTarot]),
                piles: game.piles->ArrayAux.update(j, p => p->ArrayAux.removeLast),
              })
          }
        | _ => ()
        }
      })

      newGame.contents->Option.getOr(game)
    })

    newGame.contents->Option.isSome
  }

  module Independent = {
    @react.component
    let make = (~setRef, ~onMouseDown) => {
      <React.Fragment>
        <div
          key={Free->spaceToString}
          ref={ReactDOM.Ref.callbackDomRef(setRef(TarotUp))}
          className="absolute border border-slate-200 bg-slate-700 rounded w-14 h-20"
          style={{
            top: "0px",
            left: "0px",
            zIndex: "0",
          }}
        />
        <div
          key={Free->spaceToString}
          ref={ReactDOM.Ref.callbackDomRef(setRef(TarotDown))}
          className="absolute border border-slate-200 bg-slate-700 rounded w-14 h-20"
          style={{
            top: "0px",
            left: (foundationOffset - 30 - 70 * 2)->Int.toString ++ "px",
            zIndex: "0",
          }}
        />
        <div
          key={Free->spaceToString}
          ref={ReactDOM.Ref.callbackDomRef(setRef(Free))}
          className="absolute border border-purple-200 bg-purple-100 rounded w-14 h-20"
          style={{
            top: "0px",
            left: (foundationOffset - 70)->Int.toString ++ "px",
            zIndex: "0",
            transform: "rotate(90deg)",
          }}
        />
        {[[], [], [], []]
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Foundation(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
            className="absolute border border-slate-200 bg-slate-100 rounded w-14 h-20"
            style={{
              top: "0px",
              left: (foundationOffset + 30 + i * 70)->Int.toString ++ "px",
              zIndex: "0",
            }}
          />
        })
        ->React.array}
        {[[], [], [], [], [], [], [], [], [], [], []]
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Pile(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
            className="absolute border border-slate-200 bg-slate-100  rounded w-14 h-20"
            style={{
              top: "100px",
              left: (i * 70)->Int.toString ++ "px",
              zIndex: "0",
            }}
          />
        })
        ->React.array}
        {fullDeck
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

  module Dependent = {
    @react.component
    let make = (~setGame as _, ~moveToState as _, ~autoProgress as _, ~game as _) => {
      React.null
    }
  }
}

// module Game = GameBase.GameBase(GameRules)
