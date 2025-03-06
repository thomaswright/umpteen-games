open Webapi.Dom

let getShuffledDeck = () => {
  Card.allRanks
  ->Array.reduce([], (a, rank) => {
    Card.allSuits->Array.reduce(a, (a2, suit) => {
      a2->Array.concat([
        (
          {
            suit,
            rank,
          }: Card.card
        ),
      ])
    })
  })
  ->Array.toShuffled
}

let shuffledDeck = getShuffledDeck()

@decco
type space = Card(Card.card) | Foundation(int) | Pile(int)

type game = {
  piles: array<array<Card.card>>,
  foundations: array<array<Card.card>>,
  stock: array<Card.card>,
  waste: array<Card.card>,
  gameEnded: bool,
}

let initiateGame = () => {
  // let shuffledDeck = getShuffledDeck()

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

// @set external setSpace: (Dom.element, string) => unit = "space"

@set @scope("style") external setStyleLeft: (Dom.element, string) => unit = "left"
@set @scope("style") external setStyleTop: (Dom.element, string) => unit = "top"
@set @scope("style") external setStyleZIndex: (Dom.element, string) => unit = "z-index"

@val @scope("performance") external now: unit => float = "now"
@val external requestAnimationFrame: (float => unit) => unit = "requestAnimationFrame"

@val @module("./other.js")
external appendReactElement: (React.element, string) => unit = "appendReactElement"

let zIndexFromElement = element => {
  Obj.magic(element)["style"]["z-index"]->Int.fromString
}

let getSpace = element => {
  switch element->Element.id->Js.Json.parseExn->space_decode {
  | Ok(d) => Some(d)
  | _ => None
  }
}

let spaceToString = space => {
  space->space_encode->Js.Json.stringify
}

type position = {
  top: float,
  right: float,
  bottom: float,
  left: float,
}

let elementPosition = element => {
  let a = element->Element.getBoundingClientRect

  {top: a->DomRect.top, right: a->DomRect.right, bottom: a->DomRect.bottom, left: a->DomRect.left}
}

let eventPosition = event => {
  event
  ->JsxEvent.Mouse.currentTarget
  ->Obj.magic
  ->elementPosition
}

module CardDisplay = {
  @react.component
  let make = (~card, ~id, ~cardRef, ~onMouseDown) => {
    <div
      id={id}
      ref={cardRef}
      onMouseDown={onMouseDown}
      className="absolute w-14 h-20 select-none"
      // style={{
      //   top,
      //   left,
      //   zIndex,
      // }}
    >
      <div
        style={{
          // transform: Card.rotation(card),
          // position: "relative",
          // zIndex: ((isDragging ? 100 : 0) + index + 1)->Int.toString,
          color: card->Card.colorHex,
        }}
        className={[
          " border border-gray-300 rounded w-14 h-20 bg-white shadow-sm px-1 leading-none py-0.5 cursor-default",
          // index == 0 ? "" : "-ml-[37px]",
        ]->Array.join(" ")}>
        // <div className={" bg-blue-500 h-2 w-2 rotate-45"}> {""->React.string} </div>
        <span className="flex flex-col">
          <span className="flex flex-row">
            <span
              className={[
                "font-medium ",
                switch card.rank {
                | R10 => "tracking-[-0.1rem] w-4"
                | _ => "w-3.5"
                },
              ]->Array.join(" ")}>
              {card->Card.rankString->React.string}
            </span>
            <span className="w-3.5 flex flex-row justify-center">
              {card->Card.suitString->React.string}
            </span>
          </span>
          <span className="w-3.5 flex flex-row mt-0.5 -ml-0.5">
            {card->Card.suitString->React.string}
          </span>
        </span>
      </div>
    </div>
  }
}

type cardLoc = {
  card: Card.card,
  x: int,
  y: int,
  z: int,
}

let initialGame = initiateGame()

type state = {history: array<game>}

type undoStats = {
  currentUndoDepth: int,
  undos: array<int>,
}

@react.component
let make = () => {
  let undoStats = React.useRef({
    currentUndoDepth: 0,
    undos: [],
  })

  let setUndoStats = f => {
    undoStats.current = f(undoStats.current)
  }

  let state = React.useRef({
    history: [initialGame],
  })

  let setState = f => {
    state.current = f(state.current)
  }

  let getGame = () =>
    state.current.history->Array.getUnsafe(state.current.history->Array.length - 1)

  let setGame = f => {
    if undoStats.current.currentUndoDepth > 0 {
      setUndoStats(undoStats => {
        currentUndoDepth: 0,
        undos: Array.concat(undoStats.undos, [undoStats.currentUndoDepth]),
      })
    }

    setState(state => {
      let newGame = f(getGame())
      {
        history: Array.concat(state.history, [newGame]),
      }
    })
  }

  let _undo = () => {
    if state.current.history->Array.length > 1 {
      setState(state => {
        {
          history: state.history->Array.slice(~start=0, ~end=state.history->Array.length - 1),
        }
      })
      setUndoStats(undoStats => {
        ...undoStats,
        currentUndoDepth: undoStats.currentUndoDepth + 1,
      })
    }
  }

  let refs = React.useRef([])

  let dragCard: React.ref<option<Dom.element>> = React.useRef(None)
  let offset = React.useRef((0, 0))
  let originalData = React.useRef(None)

  let getElement = a => refs.current->Array.find(el => el->getSpace == Some(a))

  let cardLocs = (game: game) => {
    // Todo: maybe change to a map
    let cards = ref([])
    let addToCards = card => cards := Array.concat(cards.contents, [card])
    game.piles->Array.forEachWithIndex((pile, i) => {
      pile->Array.forEachWithIndex((card, j) => {
        addToCards({
          card,
          x: i * 70,
          y: 200 + j * 20,
          z: j + 1,
        })
      })
    })

    game.foundations->Array.forEachWithIndex((pile, i) => {
      pile->Array.forEachWithIndex((card, j) => {
        addToCards({
          card,
          x: i * 70,
          y: 100,
          z: j + 1,
        })
      })
    })

    game.stock->Array.forEachWithIndex((card, i) => {
      addToCards({
        card,
        x: 0,
        y: 0,
        z: i + 1,
      })
    })

    game.waste->Array.forEachWithIndex((card, i) => {
      addToCards({
        card,
        x: 70,
        y: 0,
        z: i + 1,
      })
    })

    cards.contents
  }

  let setRef = card => (element: Js.Nullable.t<Dom.element>) => {
    switch element {
    | Value(a) => {
        a->Element.setId(card->spaceToString)

        refs.current->Array.push(a)
      }
    | Null => ()
    | Undefined => ()
    }
  }

  let applyToChildren = (element, f) => {
    switch element->getSpace {
    | Some(Card(card)) => {
        let game = getGame()

        game.foundations->Array.forEach(stack => {
          stack->Array.forEachWithIndex((sCard, i) => {
            if card == sCard {
              stack
              ->Array.get(i + 1)
              ->Option.flatMap(x => Card(x)->getElement)
              ->Option.mapOr(
                (),
                childEl => {
                  f(childEl)
                },
              )
            }
          })
        })

        game.piles->Array.forEach(stack => {
          stack->Array.forEachWithIndex((sCard, i) => {
            if card == sCard {
              stack
              ->Array.get(i + 1)
              ->Option.flatMap(x => Card(x)->getElement)
              ->Option.mapOr(
                (),
                childEl => {
                  f(childEl)
                },
              )
            }
          })
        })

        // game.stock->Array.forEach(sCard => {
        //   check(sCard)
        // })

        // game.waste->Array.forEach(sCard => {
        //   check(sCard)
        // })
      }
    | _ => ()
    }

    // refs.current->Array.forEach(el => {
    //   switch (elementSpace, el->parentFromElement) {
    //   | (Some(Card(elementCard)), Some(Card(parentCard))) =>
    //     if Card.equals(parentCard, elementCard) {
    //       f(el)
    //     }
    //   | _ => ()
    //   }
    // })
  }

  let rec move = (element, left, top, zIndex, offset) => {
    element->setStyleLeft(left->Int.toString ++ "px")
    element->setStyleTop(top->Int.toString ++ "px")
    zIndex->Option.mapOr((), zIndex => {
      element->setStyleZIndex(zIndex->Int.toString)
    })

    offset->Option.mapOr((), ((leftOffset, topOffset)) => {
      applyToChildren(element, childEl =>
        move(
          childEl,
          left + leftOffset,
          top + topOffset,
          zIndex->Option.map(zIndex => zIndex + 1),
          offset,
        )
      )
    })
  }

  let moveWithTime = (element, targetLeft, targetTop, zIndex, offset, duration) => {
    let start = element->elementPosition
    let startTime = now()

    let rec step: float => unit = currentTime => {
      let elapsedTime = currentTime -. startTime
      let progress = Math.min(elapsedTime /. duration, 1.) // Clamp progress between 0 and 1
      // let easedProgress = easeOutQuad(progress)
      let easedProgress = progress
      let leftMove = start.left +. (targetLeft -. start.left) *. easedProgress
      let topMove = start.top +. (targetTop -. start.top) *. easedProgress
      move(element, leftMove->Int.fromFloat, topMove->Int.fromFloat, zIndex, offset)

      if progress < 1. {
        requestAnimationFrame(step)
      }
    }

    requestAnimationFrame(step)
  }

  let moveToState = () => {
    cardLocs(getGame())->Array.forEach(a => {
      switch getElement(Card(a.card)) {
      | None => ()
      | Some(element) =>
        moveWithTime(element, a.x->Int.toFloat, a.y->Int.toFloat, Some(a.z), None, 100.)
      }
    })
  }

  let rec liftUp = (element, zIndex) => {
    element->setStyleZIndex(zIndex->Int.toString)
    applyToChildren(element, childEl => {
      liftUp(childEl, zIndex + 1)
    })
  }

  let getOverlap = (aEl, bEl) => {
    let aPos = aEl->elementPosition
    let bPos = bEl->elementPosition

    let overlapX = Math.max(0., Math.min(aPos.right, bPos.right) -. Math.max(aPos.left, bPos.left))
    let overlapY = Math.max(0., Math.min(aPos.bottom, bPos.bottom) -. Math.max(aPos.top, bPos.top))

    overlapX *. overlapY
  }

  let baseSpace = (dropCard: Card.card) => {
    let game = getGame()

    let base = ref(None)

    game.piles->Array.forEachWithIndex((pile, i) => {
      pile->Array.forEach(card => {
        if card == dropCard {
          base := Some(Pile(i))
        }
      })
    })

    game.foundations->Array.forEachWithIndex((pile, i) => {
      pile->Array.forEach(card => {
        if card == dropCard {
          base := Some(Foundation(i))
        }
      })
    })

    base.contents
  }

  let buildDragPile = card => {
    let dragPile = ref([])
    let game = getGame()

    game.piles->Array.forEach(pile => {
      pile->Array.forEachWithIndex((pileCard, j) => {
        if pileCard == card {
          dragPile := pile->Array.sliceToEnd(~start=j)
        }
      })
    })

    game.foundations->Array.forEach(pile => {
      pile->Array.forEachWithIndex((pileCard, j) => {
        if pileCard == card {
          dragPile := pile->Array.sliceToEnd(~start=j)
        }
      })
    })

    game.waste->Array.forEach(wasteCard => {
      if wasteCard == card {
        dragPile := [card]
      }
    })

    dragPile.contents
  }

  let canDrag = (dragPile: array<Card.card>) => {
    let (dragPileIsValid, _) =
      dragPile
      ->Array.toReversed
      ->Array.reduce((true, None), ((isStillValid, onTop), onBottom) => {
        !isStillValid
          ? (false, None)
          : switch onTop {
            | None => (true, Some(onBottom))
            | Some(onTop) => (
                Card.rankIsBelow(onTop, onBottom) && onTop->Card.color != onBottom->Card.color,
                Some(onBottom),
              )
            }
      })

    dragPileIsValid
  }

  let canDrop = (dragCard: Card.card, dropSpace: space) => {
    switch dropSpace {
    | Card(dropCard) =>
      switch baseSpace(dropCard) {
      | Some(Foundation(_)) =>
        Card.rankIsBelow(dropCard, dragCard) && dragCard.suit == dropCard.suit
      | Some(Pile(_)) =>
        Card.rankIsAbove(dropCard, dragCard) && dragCard->Card.color != dropCard->Card.color
      | _ => false
      }
    | Foundation(_) => dragCard.rank == RA
    | Pile(_) => dragCard.rank == RK
    }
  }

  let onMouseDown = event => {
    let eventElement =
      event
      ->JsxEvent.Mouse.currentTarget
      ->Obj.magic

    switch eventElement->getSpace {
    | Some(Card(card)) => {
        let dragPile = buildDragPile(card)

        let canDrag = canDrag(dragPile)

        if canDrag {
          dragCard.current = eventElement->Some

          let dragCardPos = eventElement->elementPosition

          originalData.current = eventElement->zIndexFromElement->Option.map(v => (dragCardPos, v))

          liftUp(eventElement, 1000)

          let pos = event->eventPosition

          offset.current = (
            event->JsxEvent.Mouse.clientX - pos.left->Int.fromFloat,
            event->JsxEvent.Mouse.clientY - pos.top->Int.fromFloat,
          )
        }
      }
    | _ => ()
    }
  }

  let onMouseMove = event => {
    dragCard.current->Option.mapOr((), dragCard => {
      let (offsetX, offsetY) = offset.current
      let leftMove = event->MouseEvent.clientX - offsetX
      let topMove = event->MouseEvent.clientY - offsetY

      move(dragCard, leftMove, topMove, None, Some(0, 20))
    })
  }

  let getDragCard = () => {
    switch dragCard.current {
    | Some(dragCardEl) =>
      switch dragCardEl->getSpace {
      | Some(Card(dragCard)) => Some(dragCardEl, dragCard)
      | _ => None
      }
    | _ => None
    }
  }

  let onMouseUp = _ => {
    switch getDragCard() {
    | Some((dragCardEl, dragCard)) => {
        let dragPile = buildDragPile(dragCard)

        let dropOn =
          refs.current
          ->Array.filter(el =>
            dragPile
            ->Array.find(pileEl => Some(Card(pileEl)) == el->getSpace)
            ->Option.isNone
          )
          ->Array.reduce(None, (acc: option<(float, Dom.element)>, el) => {
            el
            ->getSpace
            ->Option.mapOr(acc, elSpace => {
              let dropHasNoChildren = switch elSpace {
              | Card(card) => buildDragPile(card)->Array.length < 2
              | _ => true
              }

              let canDrop = dropHasNoChildren && canDrop(dragCard, elSpace)

              if canDrop {
                let overlap = getOverlap(el, dragCardEl)
                let new = Some((overlap, el))

                if overlap > 0. {
                  switch acc {
                  | None => new
                  | Some((accOverlap, _)) => accOverlap > overlap ? acc : new
                  }
                } else {
                  acc
                }
              } else {
                acc
              }
            })
          })
          ->Option.map(((_, x)) => x)

        switch dropOn {
        | None => ()
        | Some(dropOnEl) =>
          // Update State
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

          switch dropOnEl->getSpace {
          | Some(Card(card)) =>
            setGame(game => {
              {
                ...game,
                foundations: game.foundations->Array.map(stack => {
                  stack->Array.reduce(
                    [],
                    (acc, sCard) => {
                      if sCard == card {
                        Array.concat(acc, Array.concat([sCard], dragPile))
                      } else {
                        Array.concat(acc, [sCard])
                      }
                    },
                  )
                }),
                piles: game.piles->Array.map(stack => {
                  stack->Array.reduce(
                    [],
                    (acc, sCard) => {
                      if sCard == card {
                        Array.concat(acc, Array.concat([sCard], dragPile))
                      } else {
                        Array.concat(acc, [sCard])
                      }
                    },
                  )
                }),
              }
            })
          | Some(Foundation(i)) =>
            setGame(game => {
              {
                ...game,
                foundations: game.foundations->Array.mapWithIndex((foundation, fi) => {
                  fi == i ? dragPile : foundation
                }),
              }
            })
          | Some(Pile(i)) =>
            setGame(game => {
              {
                ...game,
                piles: game.piles->Array.mapWithIndex((pile, pi) => {
                  pi == i ? dragPile : pile
                }),
              }
            })
          | _ => ()
          }
        }
      }
    | None => ()
    }

    moveToState()

    dragCard.current = None
  }

  React.useEffect(() => {
    window->Window.addMouseMoveEventListener(onMouseMove)
    window->Window.addMouseUpEventListener(onMouseUp)
    moveToState()
    None
  }, [])

  let dealToWaste = () => {
    setGame(game =>
      if game.stock->Array.length == 0 {
        {
          ...game,
          stock: game.waste,
          waste: [],
        }
      } else {
        {
          ...game,
          stock: game.stock->Array.sliceToEnd(~start=1),
          waste: game.waste->Array.concat(game.stock->Array.slice(~start=0, ~end=1)),
        }
      }
    )
    moveToState()
  }

  <div id={"board"} className="relative">
    <div
      // key={Stock->spaceToString}
      // ref={ReactDOM.Ref.callbackDomRef(setRef(Stock))}
      className="absolute bg-pink-500 rounded w-14 h-20"
      style={{
        top: "0px",
        left: "0px",
        zIndex: "0",
      }}
    />
    <div
      key={"stock-cover"}
      // ref={ReactDOM.Ref.callbackDomRef(setRef(Stock, None))}
      onClick={_ => dealToWaste()}
      className="absolute bg-blue-700 rounded w-14 h-20"
      style={{
        top: "0px",
        left: "0px",
        zIndex: "53",
      }}
    />
    {[[], [], [], []]
    ->Array.mapWithIndex((_, i) => {
      <div
        key={Foundation(i)->spaceToString}
        ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
        className="absolute bg-purple-500 rounded w-14 h-20"
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
        className="absolute bg-red-500 rounded w-14 h-20"
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
      <CardDisplay
        card={card}
        key={Card(card)->spaceToString}
        id={Card(card)->spaceToString}
        cardRef={ReactDOM.Ref.callbackDomRef(setRef(Card(card)))}
        // top={(200 + j * 20)->Int.toString ++ "px"}
        // left={(i * 70)->Int.toString ++ "px"}
        // zIndex={(j + 1)->Int.toString}
        onMouseDown={onMouseDown}
      />
    })
    ->React.array}
  </div>
}

// let stock = switch getElement(Some(Stock)) {
// | Some(el) => buildDragPile(el, [])
// | _ => []
// }

// let waste = switch getElement(Some(Waste)) {
// | Some(el) => buildDragPile(el, [])
// | _ => []
// }

// stock
// ->Array.get(0)
// ->Option.mapOr((), topStockEl => {
//   switch topStockEl->spaceFromElement {
//   | Some(Stock) =>
//     let wasteCards =
//       waste
//       ->Array.toReversed
//       ->Array.sliceToEnd(~start=1)

//     wasteCards->Array.forEachWithIndex((wasteCard, i) => {
//       if i == wasteCards->Array.length - 1 {
//         wasteCard->setParent(Stock->spaceToString)
//         moveWithTime(wasteCard, 0., 0., 0, 0, Some(1), 200.)
//       } else {
//         wasteCards
//         ->Array.get(i + 1)
//         ->Option.flatMap(spaceFromElement)
//         ->Option.mapOr(
//           (),
//           v => {
//             wasteCard->setParent(v->spaceToString)
//           },
//         )
//       }
//     })

//   | Some(_) => {
//       let topWasteElement = waste->Array.getUnsafe(0)

//       topWasteElement
//       ->spaceFromElement
//       ->Option.mapOr((), v => {
//         topStockEl->setParent(v->spaceToString)
//       })

//       let pos = topWasteElement->elementPosition

//       moveWithTime(
//         topStockEl,
//         pos.left,
//         pos.top,
//         0,
//         0,
//         topWasteElement->zIndexFromElement->Option.map(v => v + 1),
//         200.,
//       )
//     }
//   | _ => ()
//   }
// })

// {stockData
// ->Array.mapWithIndex((card, i) => {
//   <CardDisplay
//     card={card}
//     key={Card(card)->spaceToString}
//     cardRef={ReactDOM.Ref.callbackDomRef(setRef(Card(card)))}
//     top={0->Int.toString ++ "px"}
//     left={0->Int.toString ++ "px"}
//     zIndex={(i + 1)->Int.toString}
//     onMouseDown={onMouseDown}
//   />
// })
// ->React.array}
// <div
//   key={Waste->spaceToString}
//   ref={ReactDOM.Ref.callbackDomRef(setRef(Waste))}
//   className="absolute bg-cyan-500 rounded w-14 h-20"
//   style={{
//     top: "0px",
//     left: "70px",
//     zIndex: "0",
//   }}
// />
// {cardsData
// ->Array.mapWithIndex((cardPile, i) => {
//   cardPile
//   ->Array.mapWithIndex((card, j) => {
//     // let parent = j == 0 ? Pile(i) : Card(cardsData->Array.getUnsafe(i)->Array.getUnsafe(j - 1))

//     <CardDisplay
//       card={card}
//       key={Card(card)->spaceToString}
//       cardRef={ReactDOM.Ref.callbackDomRef(setRef(Card(card)))}
//       top={(200 + j * 20)->Int.toString ++ "px"}
//       left={(i * 70)->Int.toString ++ "px"}
//       zIndex={(j + 1)->Int.toString}
//       onMouseDown={onMouseDown}
//     />
//   })
//   ->React.array
// })
// ->React.array}

// let moveOne = (element, left, top, z) => {
//   element->setStyleLeft(left->Int.toString ++ "px")
//   element->setStyleTop(top->Int.toString ++ "px")
//   element->setStyleZIndex(z->Int.toString)
// }

// let moveOneWithTime = (element, left, top, z, duration) => {
//   let start = element->elementPosition
//   let startTime = now()

//   let rec step: float => unit = currentTime => {
//     let elapsedTime = currentTime -. startTime
//     let progress = Math.min(elapsedTime /. duration, 1.) // Clamp progress between 0 and 1
//     // let easedProgress = easeOutQuad(progress)
//     let easedProgress = progress
//     let leftMove = start.left +. (left -. start.left) *. easedProgress
//     let topMove = start.top +. (top -. start.top) *. easedProgress
//     moveOne(element, leftMove->Int.fromFloat, topMove->Int.fromFloat, z)

//     if progress < 1. {
//       requestAnimationFrame(step)
//     }
//   }

//   requestAnimationFrame(step)
// }
