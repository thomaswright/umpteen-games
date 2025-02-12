module Card = {
  type suit = Spades | Hearts | Diamonds | Clubs

  type rank =
    | RA
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7
    | R8
    | R9
    | R10
    | RJ
    | RQ
    | RK

  let allRanks = [RA, R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK]

  let allSuits = [Spades, Hearts, Diamonds, Clubs]

  type rec card = {
    suit: suit,
    rank: rank,
    revealed: bool,
    // cardOnTop: option<card>,
  }

  let isRed = (card: card) => card.suit == Hearts || card.suit == Diamonds
  let isBlack = (card: card) => card.suit == Spades || card.suit == Clubs

  let rankIsBelow = (a, b) => {
    switch (a.rank, b.rank) {
    | (RA, R2)
    | (R2, R3)
    | (R3, R4)
    | (R4, R5)
    | (R5, R6)
    | (R6, R7)
    | (R7, R8)
    | (R8, R9)
    | (R9, R10)
    | (R10, RJ)
    | (RJ, RQ)
    | (RQ, RK) => true
    | _ => false
    }
  }
  let rankIsAbove = (a, b) => {
    switch (a.rank, b.rank) {
    | (R2, RA)
    | (R3, R2)
    | (R4, R3)
    | (R5, R4)
    | (R6, R5)
    | (R7, R6)
    | (R8, R7)
    | (R9, R8)
    | (R10, R9)
    | (RJ, R10)
    | (RQ, RJ)
    | (RK, RQ) => true
    | _ => false
    }
  }

  let rankString = card =>
    switch card.rank {
    | RA => "A"
    | R2 => "2"
    | R3 => "3"
    | R4 => "4"
    | R5 => "5"
    | R6 => "6"
    | R7 => "7"
    | R8 => "8"
    | R9 => "9"
    | R10 => "10"
    | RJ => "J"
    | RQ => "Q"
    | RK => "K"
    }

  let suitString = card =>
    switch card.suit {
    | Spades => "♠"
    | Hearts => "♥"
    | Diamonds => "♦"
    | Clubs => "♣"
    }

  let string = (card: card) => {
    <span className="flex flex-col">
      <span className="flex flex-row">
        <span
          className={[
            "text-center font-medium ",
            switch card.rank {
            | R10 => "tracking-tighter w-4 -ml-px"
            | _ => "w-3.5 "
            },
          ]->Array.join(" ")}>
          {card->rankString->React.string}
        </span>
        <span className="w-3.5 flex flex-row justify-center">
          {card->suitString->React.string}
        </span>
      </span>
      <span className="w-3.5 flex flex-row justify-center mt-0.5">
        {card->suitString->React.string}
      </span>
    </span>
  }

  let color = card =>
    switch card.suit {
    | Spades => "hsl(0 0% 0%)"
    | Hearts => "hsl(0 100% 44.31%)"
    | Diamonds => "hsl(0 100% 44.31%)"
    | Clubs => "hsl(0 0% 0%)"
    }

  let id = (card: card) => card->rankString ++ card->suitString

  let isOppositeColor = (a, b) => isRed(a) != isRed(b)

  let rotation = (card: card) => {
    let suitJitter = allSuits->Array.findIndex(s => s == card.suit) - 2
    let rankJitter = mod(allRanks->Array.findIndex(r => r == card.rank), 4) - 2

    `rotate(${(suitJitter + rankJitter)->Int.toString}deg)`
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
            revealed: false,
            // cardOnTop: None,
          }: Card.card
        ),
      ])
    })
  })
  ->Array.toShuffled
}

module Klondike = {
  type game = {
    piles: array<array<Card.card>>,
    foundations: array<array<Card.card>>,
    stock: array<Card.card>,
    waste: array<Card.card>,
    gameEnded: bool,
  }

  let canPutOnPile = (a: Card.card, b: Card.card, _) => {
    a.revealed && b.revealed && Card.rankIsAbove(a, b) && Card.isOppositeColor(a, b)
  }

  let canPutOnFoundation = (a: Card.card, b: Card.card, hasOnTop) => {
    // a.cardOnTop->Option.isNone &&
    !hasOnTop && a.revealed && b.revealed && Card.rankIsBelow(a, b) && a.suit == b.suit
  }

  let initiateGame = () => {
    let shuffledDeck = getShuffledDeck()

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
}

type useDroppableOutput = {setNodeRef: ReactDOM.Ref.callbackDomRef, isOver: bool}
// type useDroppableInputData = {
//   accepts: array<string>
// }
type useDroppableInput = {id: string}

@module("@dnd-kit/core")
external useDroppable: useDroppableInput => useDroppableOutput = "useDroppable"

type useDraggableOutput = {
  isDragging: bool,
  setNodeRef: ReactDOM.Ref.callbackDomRef,
  listeners: {"onPointerDown": ReactEvent.Pointer.t => unit},
  transform: Nullable.t<{"x": int, "y": int}>,
}

type useDraggableInput = {id: string}

@module("@dnd-kit/core")
external useDraggable: useDraggableInput => useDraggableOutput = "useDraggable"

type dragEndEvent = {"over": {"id": string}, "active": {"id": string}}

type dragStartEvent = {"active": {"id": string}}

// type dndProps = {

// }

module DraggableTest = {
  @react.component @module("./Test.jsx")
  external make: unit => Jsx.element = "default"
}

module DndContext = {
  @react.component @module("@dnd-kit/core")
  external make: (
    ~children: Jsx.element,
    ~onDragEnd: dragEndEvent => unit,
    ~onDragStart: dragStartEvent => unit,
    ~onDragCancel: unit => unit,
  ) => Jsx.element = "DndContext"
}

module DropZone = {
  @react.component
  let make = (~canDrop: bool, ~empty: bool=false, ~cardId: string) => {
    let {setNodeRef, isOver} = useDroppable({
      id: cardId,
    })
    // {
    //     "canDrop": canDrop,
    //     "drop": onDrop,
    //     "accept": "CARD",
    //     "collect": (monitor: monitor) => {
    //       beingDragged: monitor.getClientOffset(),
    //       isOver: monitor.isOver(),
    //       canDrop: monitor.canDrop(),
    //     },
    //   }

    <div
      ref={ReactDOM.Ref.callbackDomRef(setNodeRef)}
      className={[
        "rounded h-[80px] w-[57px]",
        switch (empty, isOver, canDrop) {
        | (false, true, true) => "opacity-0 bg-blue-200"
        | (false, true, false) => "opacity-0 bg-red-200"
        | (false, false, _) => "opacity-0"
        | (true, true, true) => "opacity-0 bg-blue-200"
        | (true, true, false) => "opacity-0 bg-red-200"
        | (true, false, _) => "bg-gray-200 "
        },
      ]->Array.join(" ")}
    />
  }
}

type place = Pile | Foundation

type dropLoc =
  | PileBase(int)
  | FoundationBase(int)
  | PileChild(int, int)
  | FoundationChild(int, int)
  | Waste(int)

let encodeDropId = (d: dropLoc) => {
  switch d {
  | PileBase(num) => ["PileBase", num->Int.toString]
  | FoundationBase(num) => ["FoundationBase", num->Int.toString]
  | PileChild(num, index) => ["PileChild", num->Int.toString, index->Int.toString]
  | FoundationChild(num, index) => ["FoundationChild", num->Int.toString, index->Int.toString]
  | Waste(index) => ["Waste", index->Int.toString]
  }->Array.join("-")
}

let decodeDropId = d => {
  let split = d->String.split("-")

  switch split {
  | ["PileBase", num] =>
    switch num->Int.fromString {
    | Some(n) => PileBase(n)->Some
    | _ => None
    }

  | ["FoundationBase", num] =>
    switch num->Int.fromString {
    | Some(n) => FoundationBase(n)->Some
    | _ => None
    }

  | ["PileChild", num, index] =>
    switch (num->Int.fromString, index->Int.fromString) {
    | (Some(n), Some(i)) => PileChild(n, i)->Some
    | _ => None
    }

  | ["FoundationChild", num, index] =>
    switch (num->Int.fromString, index->Int.fromString) {
    | (Some(n), Some(i)) => FoundationChild(n, i)->Some
    | _ => None
    }
  | ["Waste", index] =>
    switch index->Int.fromString {
    | Some(n) => Waste(n)->Some
    | _ => None
    }
  | _ => None
  }
}

module type CardComp = {
  type props = {
    stack: array<Card.card>,
    index: int,
    num: int,
    place: place,
    canPutCardOnCard: (Card.card, Card.card, bool) => bool,
    aligned: bool,
    movingPile: bool,
  }
  let make: React.component<props>
}

module rec CardComp: CardComp = {
  type props = {
    stack: array<Card.card>,
    index: int,
    num: int,
    place: place,
    canPutCardOnCard: (Card.card, Card.card, bool) => bool,
    aligned: bool,
    movingPile: bool,
  }

  let make: React.component<props> = ({
    stack,
    canPutCardOnCard,
    aligned,
    num,
    index,
    place,
    movingPile,
  }) => {
    let card = stack->Array.getUnsafe(index)
    let onTop = stack->Array.get(index + 1)
    let hasOnTop = onTop->Option.isSome

    let cardId = encodeDropId(
      switch place {
      | Pile => PileChild(num, index)
      | Foundation => FoundationChild(num, index)
      },
    )

    let {isDragging: _, setNodeRef, listeners, transform} = useDraggable({
      id: cardId,
    })

    let style =
      transform
      ->Nullable.toOption
      ->Option.mapOr(
        (
          {
            transform: `translate3d(0px, 0px, 0px)`,
            zIndex: movingPile ? "2" : "1",
          }: JsxDOMStyle.t
        ),
        t => {
          transform: `translate3d(${t["x"]->Int.toString}px, ${t["y"]->Int.toString}px, 0px)`,
          zIndex: "2",
        },
      )

    <div
      ref={ReactDOM.Ref.callbackDomRef(setNodeRef)}
      onPointerDown={listeners["onPointerDown"]}
      style={style}>
      <div
        style={{
          // transform: Card.rotation(card),
          // position: "relative",
          // position: place == Foundation ? "relative" : "static",
          // zIndex: (isDragging ? 100 : 0)->Int.toString,
          color: card->Card.color,
        }}
        className={[
          " border border-gray-300 rounded h-[80px] w-[57px] bg-white shadow-sm px-1 leading-none py-0.5 cursor-default",
          place == Pile ? "-mb-[58px]" : "-mb-[80px]",
        ]->Array.join(" ")}>
        // <div className={" bg-blue-500 h-2 w-2 rotate-45"}> {""->React.string} </div>
        {card->Card.string}
      </div>
      {hasOnTop
        ? <CardComp movingPile aligned stack canPutCardOnCard index={index + 1} num place />
        : <DropZone canDrop={true} cardId={cardId} />}
    </div>
  }
}

module WasteCard = {
  @react.component
  let make = (~card: Card.card, ~index) => {
    let cardId = encodeDropId(Waste(index))

    let {isDragging: _, setNodeRef, listeners, transform} = useDraggable({
      id: cardId,
    })

    let style =
      transform
      ->Nullable.toOption
      ->Option.mapOr(({}: JsxDOMStyle.t), t => {
        transform: `translate3d(${t["x"]->Int.toString}px, ${t["y"]->Int.toString}px, 0)`,
      })

    <div
      ref={ReactDOM.Ref.callbackDomRef(setNodeRef)}
      onPointerDown={listeners["onPointerDown"]}
      style={style}>
      <div
        style={{
          // transform: Card.rotation(card),
          position: "relative",
          // zIndex: ((isDragging ? 100 : 0) + index + 1)->Int.toString,
          color: card->Card.color,
        }}
        className={[
          " border border-gray-300 rounded h-[80px] w-[57px] bg-white shadow-sm px-1 leading-none py-0.5 cursor-default",
          index == 0 ? "" : "-ml-[37px]",
        ]->Array.join(" ")}>
        // <div className={" bg-blue-500 h-2 w-2 rotate-45"}> {""->React.string} </div>
        {card->Card.string}
      </div>
    </div>
  }
}

module Pile = {
  @react.component
  let make = (~num, ~stack: array<Card.card>, ~moving) => {
    let movingPile = switch moving {
    | Some(PileChild(movingNum, _)) => movingNum == num
    | _ => false
    }

    // canDrop={card => card.rank == RK}
    // <div>
    stack->Array.length != 0
      ? <CardComp
          movingPile
          aligned={false}
          index={0}
          stack
          num
          place={Pile}
          canPutCardOnCard={Klondike.canPutOnPile}
        />
      : <DropZone canDrop={true} empty={true} cardId={encodeDropId(PileBase(num))} />
    // </div>
  }
}

module Foundation = {
  @react.component
  let make = (~num, ~stack: array<Card.card>) => {
    // canDrop={card => card.rank == RA}
    <div>
      {stack->Array.length != 0
        ? <CardComp
            aligned={true}
            index={0}
            stack
            num
            canPutCardOnCard={Klondike.canPutOnFoundation}
            place={Foundation}
            movingPile={false}
          />
        : <DropZone canDrop={true} empty={true} cardId={encodeDropId(FoundationBase(num))} />}
    </div>
  }
}

module Stock = {
  @react.component
  let make = (~onClick) => {
    <button onClick={_ => onClick()}> {"Stock"->React.string} </button>
  }
}

module Waste = {
  @react.component
  let make = (~waste) => {
    Console.log(waste)
    <div className={"flex flex-row"}>
      {waste
      ->Array.mapWithIndex((wasteCard: Card.card, index) => {
        <WasteCard card={wasteCard} index={index} />
      })
      ->React.array}
    </div>
  }
}

let initialGame = Klondike.initiateGame()

type state = {history: array<Klondike.game>}

type undoStats = {
  currentUndoDepth: int,
  undos: array<int>,
}

@react.component
let make = () => {
  let (moving, setMoving) = React.useState(() => None)
  let (undoStats, setUndoStats) = React.useState((): undoStats => {
    currentUndoDepth: 0,
    undos: [],
  })
  let (state, setState) = React.useState(() => {
    history: [initialGame],
  })

  let game = state.history->Array.getUnsafe(state.history->Array.length - 1)

  let setGame = f => {
    if undoStats.currentUndoDepth > 0 {
      setUndoStats(undoStats => {
        currentUndoDepth: 0,
        undos: Array.concat(undoStats.undos, [undoStats.currentUndoDepth]),
      })
    }

    setState(state => {
      let newGame = f(game)
      {
        history: Array.concat(state.history, [newGame]),
      }
    })
  }

  let undo = () => {
    if state.history->Array.length > 1 {
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

  let {piles, foundations, gameEnded} = game

  let pileGet = (a, b) => piles->Array.getUnsafe(a)->Array.getUnsafe(b)
  let pileSize = a => piles->Array.getUnsafe(a)->Array.length
  let pileSlice = (a, b) => piles->Array.getUnsafe(a)->Array.sliceToEnd(~start=b)
  let foundationGet = (a, b) => foundations->Array.getUnsafe(a)->Array.getUnsafe(b)
  let _foundationSize = a => foundations->Array.getUnsafe(a)->Array.length

  let restart = _ => {
    setUndoStats(_ => {currentUndoDepth: 0, undos: []})
    setState(s => {history: [s.history->Array.getUnsafe(0)]})
  }

  let onDragEnd = (dragEndEvent: dragEndEvent) => {
    let dropSpace = decodeDropId(dragEndEvent["over"]["id"])
    let dragSpace = decodeDropId(dragEndEvent["active"]["id"])

    // Console.log3("DragEnd", dropSpace, dragSpace)

    switch dragSpace {
    | Some(PileChild(dragNum, dragIndex)) =>
      let dragPileSize = pileSize(dragNum)
      let dragCard = pileGet(dragNum, dragIndex)
      let dragHasChildren = dragIndex < dragPileSize - 1
      let dragSlice = pileSlice(dragNum, dragIndex)

      switch dropSpace {
      | Some(PileBase(dropNum)) =>
        // move
        setGame(game => {
          ...game,
          piles: game.piles->Array.mapWithIndex((pile, i) => {
            if i == dragNum {
              pile->Array.slice(~start=0, ~end=dragIndex)
            } else if i == dropNum {
              Array.concat(pile, dragSlice)
            } else {
              pile
            }
          }),
        })
      | Some(FoundationBase(dropNum)) =>
        if dragCard.rank == RA && !dragHasChildren {
          // move
          setGame(game => {
            ...game,
            piles: game.piles->Array.mapWithIndex((pile, i) => {
              if i == dragNum {
                pile->Array.slice(~start=0, ~end=dragIndex)
              } else {
                pile
              }
            }),
            foundations: game.foundations->Array.mapWithIndex((foundation, i) => {
              if i == dropNum {
                Array.concat(foundation, dragSlice)
              } else {
                foundation
              }
            }),
          })
        }
      | Some(PileChild(dropNum, dropIndex)) => {
          let dropPileSize = pileSize(dropNum)
          let dropCard = pileGet(dropNum, dropIndex)
          let dropHasChildren = dropIndex < dropPileSize - 1

          if (
            Card.rankIsBelow(dragCard, dropCard) &&
            Card.isOppositeColor(dragCard, dropCard) &&
            !dropHasChildren
          ) {
            // move
            setGame(game => {
              ...game,
              piles: game.piles->Array.mapWithIndex((pile, i) => {
                if i == dragNum {
                  pile->Array.slice(~start=0, ~end=dragIndex)
                } else if i == dropNum {
                  Array.concat(pile, dragSlice)
                } else {
                  pile
                }
              }),
            })
          }
        }
      | Some(FoundationChild(dropNum, dropIndex)) => {
          let dropCard = foundationGet(dropNum, dropIndex)

          if (
            Card.rankIsAbove(dragCard, dropCard) &&
            dragCard.suit == dropCard.suit &&
            !dragHasChildren
          ) {
            // move
            setGame(game => {
              ...game,
              piles: game.piles->Array.mapWithIndex((pile, i) => {
                if i == dragNum {
                  pile->Array.slice(~start=0, ~end=dragIndex)
                } else {
                  pile
                }
              }),
              foundations: game.foundations->Array.mapWithIndex((foundation, i) => {
                if i == dropNum {
                  Array.concat(foundation, dragSlice)
                } else {
                  foundation
                }
              }),
            })
          }
        }
      | _ => ()
      }
    | _ => ()
    }
    setMoving(_ => None)
  }

  let sum = arr => arr->Array.reduce(0, (a, c) => a + c)
  let max = arr => arr->Array.reduce(0., (a, c) => Math.max(a, c->Int.toFloat))->Int.fromFloat

  let dealFromStock = () => {
    setGame(game => {
      let numDealt = game.stock->Array.length > 3 ? 3 : game.stock->Array.length
      {
        ...game,
        stock: game.stock->Array.sliceToEnd(~start=numDealt),
        waste: game.stock->Array.slice(~start=0, ~end=numDealt),
      }
    })
  }

  let onDragStart = event => {
    setMoving(_ => decodeDropId(event["active"]["id"]))
  }

  let onDragCancel = () => {
    setMoving(_ => None)
  }

  <DndContext onDragEnd={onDragEnd} onDragStart onDragCancel>
    <div className="p-6">
      <div>
        <button className={"bg-gray-900 text-white px-2 rounded text-sm"} onClick={restart}>
          {"Restart"->React.string}
        </button>
        <button
          className={"bg-gray-900 text-white px-2 rounded text-sm ml-1"} onClick={_ => undo()}>
          {"Undo"->React.string}
        </button>
        <div className={"flex flex-col text-xs bg-gray-200 p-1 py-2 w-40 rounded-lg my-1"}>
          <div className={" px-2 flex flex-row gap-2"}>
            {("# Moves: " ++ (state.history->Array.length - 1)->Int.toString)->React.string}
          </div>
          <div className={" px-2 flex flex-row gap-2"}>
            {("# Undos: " ++ (undoStats.currentUndoDepth + undoStats.undos->sum)->Int.toString)
              ->React.string}
          </div>
          <div className={" px-2 flex flex-row gap-2"}>
            {("# Undo Branches: " ++
            (undoStats.undos->Array.length + (undoStats.currentUndoDepth > 0 ? 1 : 0))
              ->Int.toString)->React.string}
          </div>
          <div className={" px-2 flex flex-row gap-2"}>
            {("Max Undo Depth: " ++
            Array.concat(undoStats.undos, [undoStats.currentUndoDepth])
            ->max
            ->Int.toString)->React.string}
          </div>
        </div>
        <div> {gameEnded ? "You win!"->React.string : React.null} </div>
      </div>
      <div className={"flex flex-row gap-2 py-1"}>
        <Stock onClick={dealFromStock} />
        <Waste waste={game.waste} />
      </div>
      <div className={"flex flex-row gap-2 py-1"}>
        <Foundation stack={foundations->Array.getUnsafe(0)} num={0} />
        <Foundation stack={foundations->Array.getUnsafe(1)} num={1} />
        <Foundation stack={foundations->Array.getUnsafe(2)} num={2} />
        <Foundation stack={foundations->Array.getUnsafe(3)} num={3} />
      </div>
      <div className={"flex flex-row gap-2 py-1"}>
        <Pile moving stack={piles->Array.getUnsafe(0)} num={0} />
        <Pile moving stack={piles->Array.getUnsafe(1)} num={1} />
        <Pile moving stack={piles->Array.getUnsafe(2)} num={2} />
        <Pile moving stack={piles->Array.getUnsafe(3)} num={3} />
        <Pile moving stack={piles->Array.getUnsafe(4)} num={4} />
        <Pile moving stack={piles->Array.getUnsafe(5)} num={5} />
        <Pile moving stack={piles->Array.getUnsafe(6)} num={6} />
      </div>
    </div>
  </DndContext>
}
