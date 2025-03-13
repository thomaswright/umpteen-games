open Webapi.Dom
open Types
open Common

module GameRules = {
  let foundationOffset = 40 + 70 * 4

  let shuffledDeck = Card.getShuffledDeck()

  @decco
  type space = Card(Card.card) | Foundation(int) | Pile(int) | Free(int)

  let getSpace = element => {
    switch element->Element.id->Js.Json.parseExn->space_decode {
    | Ok(d) => Some(d)
    | _ => None
    }
  }

  let spaceToString = space => {
    space->space_encode->Js.Json.stringify
  }

  type game = {
    piles: array<array<Card.card>>,
    foundations: array<array<Card.card>>,
    free: array<array<Card.card>>,
    gameEnded: bool,
  }

  let initiateGame = () => {
    {
      piles: [
        shuffledDeck->Array.slice(~start=0, ~end=7),
        shuffledDeck->Array.slice(~start=7, ~end=14),
        shuffledDeck->Array.slice(~start=14, ~end=21),
        shuffledDeck->Array.slice(~start=21, ~end=28),
        shuffledDeck->Array.slice(~start=28, ~end=34),
        shuffledDeck->Array.slice(~start=34, ~end=40),
        shuffledDeck->Array.slice(~start=40, ~end=46),
        shuffledDeck->Array.slice(~start=46, ~end=52),
      ],
      foundations: [[], [], [], []],
      free: [[], [], [], []],
      gameEnded: false,
    }
  }

  let getSpaceLocs = (game: game) => {
    // Todo: maybe change to a map
    let cards = ref([])
    let addToCards = card => cards := Array.concat(cards.contents, [card])
    game.piles->ArrayAux.forEach2((_, card, i, j) => {
      addToCards((
        Card(card),
        {
          x: i * 70,
          y: 200 + j * 20,
          z: j + 1,
        },
      ))
    })

    game.foundations->ArrayAux.forEach2((_, card, i, j) => {
      addToCards((
        Card(card),
        {
          x: foundationOffset + i * 70,
          y: 100,
          z: j + 1,
        },
      ))
    })

    game.free->ArrayAux.forEach2((_, card, i, j) => {
      addToCards((
        Card(card),
        {
          x: i * 70,
          y: 100,
          z: 1,
        },
      ))
    })

    cards.contents
  }

  let baseSpace = (dropCard: Card.card, game: game) => {
    let base = ref(None)

    game.piles->ArrayAux.forEach2((_, card, i, _) => {
      if card == dropCard {
        base := Some(Pile(i))
      }
    })

    game.foundations->ArrayAux.forEach2((_, card, i, _) => {
      if card == dropCard {
        base := Some(Foundation(i))
      }
    })

    game.free->ArrayAux.forEach2((_, card, i, _) => {
      if card == dropCard {
        base := Some(Free(i))
      }
    })

    base.contents
  }

  let buildDragPile = (card, game: game) => {
    let dragPile = ref([])

    game.piles->ArrayAux.forEach2((pile, pileCard, _, j) => {
      if pileCard == card {
        dragPile := pile->Array.sliceToEnd(~start=j)
      }
    })

    game.foundations->ArrayAux.forEach2((pile, pileCard, _, j) => {
      if pileCard == card {
        dragPile := pile->Array.sliceToEnd(~start=j)
      }
    })

    game.free->ArrayAux.forEach2((free, freeCard, _, _) => {
      if freeCard == card {
        dragPile := [card]
      }
    })

    dragPile.contents
  }

  let canDrag = (space, game) => {
    switch space {
    | Card(card) => {
        let dragPile = buildDragPile(card, game)

        let onTopIfNeeded = switch baseSpace(card, game) {
        | Some(Foundation(i)) =>
          game.foundations
          ->Array.get(i)
          ->Option.flatMap(stack => {
            stack->ArrayAux.getLast
          })
          ->Option.mapOr(false, top => {
            top == card
          })
        | Some(Pile(i)) => true
        // game.piles
        // ->Array.get(i)
        // ->Option.flatMap(stack => {
        //   stack->ArrayAux.getLast
        // })
        // ->Option.mapOr(false, top => {
        //   top == card
        // })
        | Some(Free(i)) =>
          game.free
          ->Array.get(i)
          ->Option.flatMap(x => x->Array.get(0))
          ->Option.mapOr(false, top => {
            top == card
          })
        | _ => false
        }

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

        onTopIfNeeded && dragPileIsValid
      }
    | _ => false
    }
  }

  let canDrop = (dragSpace: space, dropSpace: space, game: game) => {
    switch dragSpace {
    | Card(dragCard) => {
        let dragPile = buildDragPile(dragCard, game)

        let notInDragPile =
          dragPile
          ->Array.find(pilePiece => Card(pilePiece) == dropSpace)
          ->Option.isNone

        let dropHasNoChildren = switch dropSpace {
        | Card(card) => buildDragPile(card, game)->Array.length < 2
        | Pile(i) => game.piles->Array.getUnsafe(i)->Array.length == 0
        | Foundation(i) => game.foundations->Array.getUnsafe(i)->Array.length == 0
        | Free(i) => game.free->Array.getUnsafe(i)->Array.length == 0
        | _ => false
        }

        let canBeParent = switch dropSpace {
        | Card(dropCard) =>
          switch baseSpace(dropCard, game) {
          | Some(Foundation(_)) =>
            Card.rankIsBelow(dropCard, dragCard) && dragCard.suit == dropCard.suit
          | Some(Pile(_)) =>
            Card.rankIsAbove(dropCard, dragCard) && dragCard->Card.color != dropCard->Card.color
          | _ => false
          }
        | Foundation(_) => dragCard.rank == RA
        | Pile(_) => dragCard.rank == RK
        | Free(_) => dragPile->Array.length == 1
        | _ => false
        }

        notInDragPile && dropHasNoChildren && canBeParent
      }
    | _ => false
    }
  }

  let onDrop = (dropOnSpace, dragSpace, game, setGame) => {
    switch dragSpace {
    | Card(dragCard) => {
        let dragPile = buildDragPile(dragCard, game)

        let removeDragPile = x =>
          x->Array.filter(sCard => {
            !(dragPile->Array.some(dCard => sCard == dCard))
          })

        setGame(game => {
          {
            ...game,
            foundations: game.foundations->Array.map(removeDragPile),
            piles: game.piles->Array.map(removeDragPile),
            free: game.free->Array.map(removeDragPile),
          }
        })

        switch dropOnSpace {
        | Card(card) =>
          setGame(game => {
            {
              ...game,
              foundations: game.foundations->Array.map(stack => {
                stack->ArrayAux.insertAfter(card, dragPile)
              }),
              piles: game.piles->Array.map(stack => {
                stack->ArrayAux.insertAfter(card, dragPile)
              }),
            }
          })
        | Foundation(i) =>
          setGame(game => {
            {
              ...game,
              foundations: game.foundations->ArrayAux.update(i, _ => dragPile),
            }
          })
        | Pile(i) =>
          setGame(game => {
            {
              ...game,
              piles: game.piles->ArrayAux.update(i, _ => dragPile),
            }
          })
        | Free(i) =>
          setGame(game => {
            {
              ...game,
              free: game.free->ArrayAux.update(i, _ => dragPile),
            }
          })
        | _ => ()
        }
      }
    | _ => ()
    }
  }

  let applyMoveToOthers = (space: space, game, move) => {
    switch space {
    | Card(card) => {
        game.foundations->ArrayAux.forEach2((stack, sCard, _, j) => {
          if card == sCard {
            stack->Array.get(j + 1)->Option.mapOr((), x => move(Card(x)))
          }
        })

        game.piles->ArrayAux.forEach2((stack, sCard, _, j) => {
          if card == sCard {
            stack->Array.get(j + 1)->Option.mapOr((), x => move(Card(x)))
          }
        })
      }
    | _ => ()
    }
  }

  let autoProgress = setGame => {
    let newGame = ref(None)

    setGame(game => {
      game.foundations->Array.forEachWithIndex((foundation, i) => {
        let canMove = (c: Card.card) => {
          switch foundation->ArrayAux.getLast {
          | None => c.rank == RA
          | Some(foundationCard) =>
            Card.rankIsBelow(foundationCard, c) && foundationCard.suit == c.suit
          }
        }
        game.free->ArrayAux.forEach2(
          (_, freeCard, freeIndex, _) => {
            if newGame.contents->Option.isNone && canMove(freeCard) {
              newGame :=
                Some({
                  ...game,
                  foundations: game.foundations->ArrayAux.update(
                    i,
                    f => f->Array.concat([freeCard]),
                  ),
                  free: game.free->ArrayAux.update(freeIndex, _ => []),
                })
            }
          },
        )

        game.piles->Array.forEachWithIndex(
          (pile, j) => {
            pile
            ->ArrayAux.getLast
            ->Option.mapOr(
              (),
              pileCard => {
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
              },
            )
          },
        )
      })

      newGame.contents->Option.getOr(game)
    })

    newGame.contents->Option.isSome
  }

  module Independent = {
    @react.component
    let make = (~setRef, ~onMouseDown) => {
      <React.Fragment>
        {[[], [], [], []]
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Free(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i)))}
            className="absolute border border-slate-200 bg-slate-100 rounded w-14 h-20"
            style={{
              top: "100px",
              left: (i * 70)->Int.toString ++ "px",
              zIndex: "0",
            }}
          />
        })
        ->React.array}
        {[[], [], [], []]
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Foundation(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
            className="absolute border border-slate-200 bg-slate-100 rounded w-14 h-20"
            style={{
              top: "100px",
              left: (foundationOffset + i * 70)->Int.toString ++ "px",
              zIndex: "0",
            }}
          />
        })
        ->React.array}
        {[[], [], [], [], [], [], []]
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Pile(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
            className="absolute border border-slate-200 bg-slate-100  rounded w-14 h-20"
            style={{
              top: "200px",
              left: (i * 70)->Int.toString ++ "px",
              zIndex: "0",
            }}
          />
        })
        ->React.array}
        {shuffledDeck
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

  module Dependent = {
    @react.component
    let make = (~setGame, ~moveToState, ~autoProgress, ~game) => {
      React.null
    }
  }
}

module Game = GameBase.GameBase(GameRules)
