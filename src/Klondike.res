open Webapi.Dom
open Types
open Common

module GameRules = {
  let shuffledDeck = Card.getShuffledDeck()

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

  type game = {
    piles: array<array<Card.card>>,
    foundations: array<array<Card.card>>,
    stock: array<Card.card>,
    waste: array<Card.card>,
    gameEnded: bool,
  }

  let initiateGame = () => {
    {
      piles: [
        shuffledDeck->Array.slice(~start=0, ~end=1),
        shuffledDeck->Array.slice(~start=1, ~end=3),
        shuffledDeck->Array.slice(~start=3, ~end=6),
        shuffledDeck->Array.slice(~start=6, ~end=10),
        shuffledDeck->Array.slice(~start=10, ~end=15),
        shuffledDeck->Array.slice(~start=15, ~end=21),
        shuffledDeck->Array.slice(~start=21, ~end=28),
      ],
      foundations: [[], [], [], []],
      stock: shuffledDeck->Array.sliceToEnd(~start=28),
      waste: [],
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
          x: i * 70,
          y: 100,
          z: j + 1,
        },
      ))
    })

    game.stock->Array.forEachWithIndex((card, i) => {
      addToCards((
        Card(card),
        {
          x: 0,
          y: 0,
          z: i + 1,
        },
      ))
    })

    game.waste->Array.forEachWithIndex((card, i) => {
      addToCards((
        Card(card),
        {
          x: 70 + 20 * i,
          y: 0,
          z: i + 1,
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

    game.waste->Array.forEach(card => {
      if card == dropCard {
        base := Some(Waste)
      }
    })

    game.stock->Array.forEach(card => {
      if card == dropCard {
        base := Some(Stock)
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

    game.waste->Array.forEach(wasteCard => {
      if wasteCard == card {
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
        | Some(Pile(_)) => true
        // game.piles
        // ->Array.get(i)
        // ->Option.flatMap(stack => {
        //   stack->ArrayAux.getLast
        // })
        // ->Option.mapOr(false, top => {
        //   top == card
        // })
        | Some(Waste) =>
          game.waste
          ->ArrayAux.getLast
          ->Option.mapOr(false, top => {
            top == card
          })
        | Some(Stock) => false
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
            stock: game.stock->removeDragPile,
            waste: game.waste->removeDragPile,
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
        game.waste
        ->ArrayAux.getLast
        ->Option.mapOr(
          (),
          wasteCard => {
            if newGame.contents->Option.isNone && canMove(wasteCard) {
              newGame :=
                Some({
                  ...game,
                  foundations: game.foundations->ArrayAux.update(
                    i,
                    f => f->Array.concat([wasteCard]),
                  ),
                  waste: game.waste->ArrayAux.removeLast,
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

  module Custom = {
    let dealToWaste = async (setGame, moveToState, autoProgress) => {
      let f = _ => {
        setGame(game => {
          ...game,
          stock: game.stock->Array.sliceToEnd(~start=1),
          waste: game.waste->Array.concat(game.stock->Array.slice(~start=0, ~end=1)),
        })
        moveToState()
      }

      await numInterval(f, 200, 3)
      autoProgress()
    }

    let restock = (setGame, moveToState) => {
      setGame(game => {
        ...game,
        stock: game.waste,
        waste: game.stock,
      })
      moveToState()
    }
  }

  module Independent = {
    @react.component
    let make = (~setRef, ~onMouseDown) => {
      <React.Fragment>
        {[[], [], [], []]
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Foundation(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
            className="absolute border border-slate-200 bg-slate-100 rounded w-14 h-20"
            style={{
              top: "100px",
              left: (i * 70)->Int.toString ++ "px",
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
      <React.Fragment>
        {if game.stock->Array.length == 0 {
          <div
            key={"stock-base"}
            onClick={_ => {
              Custom.restock(setGame, moveToState)
            }}
            className="absolute bg-blue-200 rounded w-14 h-20"
            style={{
              top: "0px",
              left: "0px",
              zIndex: "53",
            }}
          />
        } else {
          <div
            key={"stock-cover"}
            onClick={_ => {
              Custom.dealToWaste(setGame, moveToState, autoProgress)->ignore
            }}
            className="absolute bg-blue-700 rounded w-14 h-20"
            style={{
              top: "0px",
              left: "0px",
              zIndex: "53",
            }}
          />
        }}
      </React.Fragment>
    }
  }
}

module Game = GameBase.GameBase(GameRules)
