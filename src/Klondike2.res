open Webapi.Dom

type cardLoc = {
  card: Card.card,
  x: int,
  y: int,
  z: int,
}

module GameRules = {
  @decco
  type space = Card(Card.card) | Foundation(int) | Pile(int) | Waste | Stock

  type game = {
    piles: array<array<Card.card>>,
    foundations: array<array<Card.card>>,
    stock: array<Card.card>,
    waste: array<Card.card>,
    gameEnded: bool,
  }

  let initiateGame = shuffledDeck => {
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
        x: 70 + 20 * mod(i, 3),
        y: 0,
        z: i + 1,
      })
    })

    cards.contents
  }

  let baseSpace = (dropCard: Card.card, game: game) => {
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

  let canDrag = (card, game) => {
    let dragPile = buildDragPile(card, game)

    let onTop = switch baseSpace(card, game) {
    | Some(Foundation(i)) =>
      game.foundations
      ->Array.get(i)
      ->Option.flatMap(stack => {
        stack->Array.toReversed->Array.get(0)
      })
      ->Option.mapOr(false, top => {
        top == card
      })
    | Some(Pile(i)) =>
      game.piles
      ->Array.get(i)
      ->Option.flatMap(stack => {
        stack->Array.toReversed->Array.get(0)
      })
      ->Option.mapOr(false, top => {
        top == card
      })
    | Some(Waste) =>
      game.waste
      ->Array.toReversed
      ->Array.get(0)
      ->Option.mapOr(false, top => {
        Console.log2(top, card)
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

    onTop && dragPileIsValid
  }

  let canDrop = (dragCard: Card.card, dropSpace: space, game: game) => {
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

  let onDrop = (dropOnSpace, dragCard, game, setGame) => {
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
    | Foundation(i) =>
      setGame(game => {
        {
          ...game,
          foundations: game.foundations->Array.mapWithIndex((foundation, fi) => {
            fi == i ? dragPile : foundation
          }),
        }
      })
    | Pile(i) =>
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

  let applyToOthers = (card, game, f) => {
    game.foundations->Array.forEach(stack => {
      stack->Array.forEachWithIndex((sCard, i) => {
        if card == sCard {
          f(stack->Array.get(i + 1))
        }
      })
    })

    game.piles->Array.forEach(stack => {
      stack->Array.forEachWithIndex((sCard, i) => {
        if card == sCard {
          f(stack->Array.get(i + 1))
        }
      })
    })
  }
}

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
  switch element->Element.id->Js.Json.parseExn->GameRules.space_decode {
  | Ok(d) => Some(d)
  | _ => None
  }
}

let spaceToString = space => {
  space->GameRules.space_encode->Js.Json.stringify
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
    <div id={id} ref={cardRef} onMouseDown={onMouseDown} className="absolute w-14 h-20 select-none">
      <div
        style={{
          transform: Card.rotation(card),
          // position: "relative",
          color: card->Card.colorHex,
        }}
        className={[
          " border border-gray-300 rounded w-14 h-20 bg-white shadow-sm px-1 leading-none py-0.5 cursor-default",
        ]->Array.join(" ")}>
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

type state = {history: array<GameRules.game>}

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
    history: [GameRules.initiateGame(shuffledDeck)],
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

  let applyToOthers = (element, f) => {
    switch element->getSpace {
    | Some(Card(card)) => {
        let game = getGame()

        let appliedF = space => {
          space
          ->Option.flatMap(x => Card(x)->getElement)
          ->Option.mapOr((), childEl => {
            f(childEl)
          })
        }

        GameRules.applyToOthers(card, game, appliedF)
      }
    | _ => ()
    }
  }

  let rec move = (element, left, top, zIndex, offset) => {
    element->setStyleLeft(left->Int.toString ++ "px")
    element->setStyleTop(top->Int.toString ++ "px")
    zIndex->Option.mapOr((), zIndex => {
      element->setStyleZIndex(zIndex->Int.toString)
    })

    offset->Option.mapOr((), ((leftOffset, topOffset)) => {
      applyToOthers(element, childEl =>
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

    let boardPos =
      document
      ->Document.getElementById("board")
      ->Option.mapOr(
        {
          top: 0.,
          left: 0.,
          bottom: 0.,
          right: 0.,
        },
        board => board->elementPosition,
      )

    let start = {
      top: start.top -. boardPos.top,
      left: start.left -. boardPos.left,
      bottom: start.bottom -. boardPos.bottom,
      right: start.right -. boardPos.right,
    }

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
    GameRules.cardLocs(getGame())->Array.forEach(a => {
      switch getElement(Card(a.card)) {
      | None => ()
      | Some(element) =>
        moveWithTime(element, a.x->Int.toFloat, a.y->Int.toFloat, Some(a.z), None, 100.)
      }
    })
  }

  let rec liftUp = (element, zIndex) => {
    element->setStyleZIndex(zIndex->Int.toString)
    applyToOthers(element, childEl => {
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

  let onMouseDown = event => {
    let eventElement =
      event
      ->JsxEvent.Mouse.currentTarget
      ->Obj.magic

    switch eventElement->getSpace {
    | Some(Card(card)) =>
      if GameRules.canDrag(card, getGame()) {
        dragCard.current = eventElement->Some

        let dragCardPos = eventElement->elementPosition

        let boardPos =
          document
          ->Document.getElementById("board")
          ->Option.mapOr(
            {
              top: 0.,
              left: 0.,
              bottom: 0.,
              right: 0.,
            },
            board => board->elementPosition,
          )

        originalData.current = eventElement->zIndexFromElement->Option.map(v => (dragCardPos, v))

        liftUp(eventElement, 1000)

        let pos = event->eventPosition

        offset.current = (
          event->JsxEvent.Mouse.clientX - pos.left->Int.fromFloat + boardPos.left->Int.fromFloat,
          event->JsxEvent.Mouse.clientY - pos.top->Int.fromFloat + boardPos.top->Int.fromFloat,
        )
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
        let dropOn =
          refs.current
          ->Array.reduce(None, (acc: option<(float, Dom.element)>, el) => {
            el
            ->getSpace
            ->Option.mapOr(acc, elSpace => {
              if GameRules.canDrop(dragCard, elSpace, getGame()) {
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
          switch dropOnEl->getSpace {
          | Some(dropOnSpace) => GameRules.onDrop(dropOnSpace, dragCard, getGame(), setGame)
          | None => ()
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
          stock: game.stock->Array.sliceToEnd(~start=3),
          waste: game.waste->Array.concat(game.stock->Array.slice(~start=0, ~end=3)),
        }
      }
    )
    moveToState()
  }

  <div id={"board"} className="relative m-5">
    <div
      key={"stock-cover"}
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
      <CardDisplay
        card={card}
        key={Card(card)->spaceToString}
        id={Card(card)->spaceToString}
        cardRef={ReactDOM.Ref.callbackDomRef(setRef(Card(card)))}
        onMouseDown={onMouseDown}
      />
    })
    ->React.array}
  </div>
}
