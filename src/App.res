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
    | R10 => "X"
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
    <span className="flex flex-row">
      <span className="w-3.5"> {card->rankString->React.string} </span>
      <span className="w-3.5 flex flex-row justify-center"> {card->suitString->React.string} </span>
    </span>
  }

  let id = (card: card) => card->rankString ++ card->suitString

  let isOppositeColor = (a, b) => isRed(a) != isRed(b)
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
    movesCounter: int,
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
      movesCounter: 0,
      gameEnded: false,
    }
  }
}

// type cardType

// type monitor = {
//   isDragging: unit => bool,
//   isOver: unit => bool,
//   canDrop: unit => bool,
//   getClientOffset: unit => bool,
// }

// type useDropRecord<'a> = {
//   "accept": string,
//   "canDrop": Card.card => bool,
//   "drop": Card.card => unit,
//   "collect": monitor => 'a,
// }

// type useDrop<'a, 'b, 'c> = (unit => useDropRecord<'a>, ('b, 'c)) => ('a, unit => unit)

// type dropZoneProps = {
//   canDrop: bool,
//   isOver: bool,
//   beingDragged: bool,
// }

// @module("react-dnd") external useDrop: useDrop<dropZoneProps, 'b, 'c> = "useDrop"

// type useDragRecord<'a> = {
//   "type": string,
//   "canDrag": unit => bool,
//   "item": Card.card,
//   "collect": monitor => 'a,
// }
// type useDrag<'a, 'b> = (unit => useDragRecord<'a>, 'b) => ('a, JsxDOM.domRef)

// type cardCompProps = {isDragging: bool}

// @module("react-dnd") external useDrag: useDrag<cardCompProps, 'b> = "useDrag"

// type dndBackend

// @module("react-dnd-html5-backend") external html5Backend: dndBackend = "HTML5Backend"

// module DndProvider = {
//   @react.component @module("react-dnd")
//   external make: (~children: Jsx.element, ~backend: dndBackend) => Jsx.element = "DndProvider"
// }

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
  ) => // ~onDragStart: dragStartEvent => unit,
  // ~onDragCancel: unit => unit,
  Jsx.element = "DndContext"
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

module type CardComp = {
  type props = {
    stack: array<Card.card>,
    index: int,
    num: int,
    place: place,
    canPutCardOnCard: (Card.card, Card.card, bool) => bool,
    aligned: bool,
  }
  let make: React.component<props>
}

type dropLoc =
  | PileBase(int)
  | FoundationBase(int)
  | PileChild(int, int)
  | FoundationChild(int, int)

let encodeDropId = (d: dropLoc) => {
  switch d {
  | PileBase(num) => ["PileBase", num->Int.toString]
  | FoundationBase(num) => ["FoundationBase", num->Int.toString]
  | PileChild(num, index) => ["PileChild", num->Int.toString, index->Int.toString]
  | FoundationChild(num, index) => ["FoundationChild", num->Int.toString, index->Int.toString]
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
  | _ => None
  }
}

module rec CardComp: CardComp = {
  type props = {
    stack: array<Card.card>,
    index: int,
    num: int,
    place: place,
    canPutCardOnCard: (Card.card, Card.card, bool) => bool,
    aligned: bool,
  }

  let make: React.component<props> = ({stack, canPutCardOnCard, aligned, num, index, place}) => {
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
      ->Option.mapOr(({}: JsxDOMStyle.t), t => {
        transform: `translate3d(${t["x"]->Int.toString}px, ${t["y"]->Int.toString}px, 0)`,
      })

    <div
      ref={ReactDOM.Ref.callbackDomRef(setNodeRef)}
      onPointerDown={listeners["onPointerDown"]}
      style={style}>
      <div
        style={{
          position: place == Foundation ? "relative" : "static",
          // zIndex: (index + 1)->Int.toString,
        }}
        className={[
          "border border-gray-300 rounded h-[80px] w-[57px] bg-white shadow-sm px-1 leading-none py-0.5 cursor-default",
          switch card.suit {
          | Spades => "text-black"
          | Hearts => "text-red-600"
          | Diamonds => "text-red-600"
          | Clubs => "text-black"
          },
          place == Pile ? "-mb-[58px]" : "-mb-[80px]",
        ]->Array.join(" ")}>
        {card->Card.string}
      </div>
      {hasOnTop
        ? <CardComp aligned stack canPutCardOnCard index={index + 1} num place />
        : <DropZone canDrop={true} cardId={cardId} />}
    </div>
  }
}

module Pile = {
  @react.component
  let make = (~num, ~stack: array<Card.card>) => {
    // canDrop={card => card.rank == RK}
    <div>
      {stack->Array.length != 0
        ? <CardComp
            aligned={false}
            index={0}
            stack
            num
            place={Pile}
            canPutCardOnCard={Klondike.canPutOnPile}
          />
        : <DropZone canDrop={true} empty={true} cardId={encodeDropId(PileBase(num))} />}
    </div>
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
          />
        : <DropZone canDrop={true} empty={true} cardId={encodeDropId(FoundationBase(num))} />}
    </div>
  }
}

@react.component
let make = () => {
  let (game, setGame) = React.useState(() => Klondike.initiateGame())

  let {piles, foundations, movesCounter, gameEnded} = game

  let pileGet = (a, b) => piles->Array.getUnsafe(a)->Array.getUnsafe(b)
  let pileSize = a => piles->Array.getUnsafe(a)->Array.length
  let pileSlice = (a, b) => piles->Array.getUnsafe(a)->Array.sliceToEnd(~start=b)
  let foundationGet = (a, b) => foundations->Array.getUnsafe(a)->Array.getUnsafe(b)
  let _foundationSize = a => foundations->Array.getUnsafe(a)->Array.length

  let restart = _ => ()

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
  }

  <DndContext onDragEnd={onDragEnd}>
    <div className="p-6">
      <div>
        <button onClick={restart}> {"restart"->React.string} </button>
        <div> {("Moves: " ++ movesCounter->Int.toString)->React.string} </div>
        <div> {gameEnded ? "You win!"->React.string : React.null} </div>
      </div>
      <div className={"flex flex-row gap-2 py-1"}>
        <Foundation stack={foundations->Array.getUnsafe(0)} num={0} />
        <Foundation stack={foundations->Array.getUnsafe(1)} num={1} />
        <Foundation stack={foundations->Array.getUnsafe(2)} num={2} />
        <Foundation stack={foundations->Array.getUnsafe(3)} num={3} />
      </div>
      <div className={"flex flex-row gap-2 py-1"}>
        <Pile stack={piles->Array.getUnsafe(0)} num={0} />
        <Pile stack={piles->Array.getUnsafe(1)} num={1} />
        <Pile stack={piles->Array.getUnsafe(2)} num={2} />
        <Pile stack={piles->Array.getUnsafe(3)} num={3} />
        <Pile stack={piles->Array.getUnsafe(4)} num={4} />
        <Pile stack={piles->Array.getUnsafe(5)} num={5} />
        <Pile stack={piles->Array.getUnsafe(6)} num={6} />
      </div>
    </div>
  </DndContext>
}
