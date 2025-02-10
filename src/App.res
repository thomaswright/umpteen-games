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
    piles: (
      array<Card.card>,
      array<Card.card>,
      array<Card.card>,
      array<Card.card>,
      array<Card.card>,
      array<Card.card>,
      array<Card.card>,
    ),
    foundations: (array<Card.card>, array<Card.card>, array<Card.card>, array<Card.card>),
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
      piles: (
        shuffledDeck->Array.slice(~start=0, ~end=1),
        shuffledDeck->Array.slice(~start=1, ~end=3),
        shuffledDeck->Array.slice(~start=3, ~end=6),
        shuffledDeck->Array.slice(~start=6, ~end=10),
        shuffledDeck->Array.slice(~start=10, ~end=15),
        shuffledDeck->Array.slice(~start=15, ~end=21),
        shuffledDeck->Array.slice(~start=21, ~end=28),
      ),
      foundations: ([], [], [], []),
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

type dragEndEvent = {"over": {"id": string}}

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
        | (false, true, true) => "bg-blue-200"
        | (false, true, false) => "bg-red-200"
        | (false, false, _) => "opacity-0"
        | (true, true, true) => "bg-blue-200"
        | (true, true, false) => "bg-red-200"
        | (true, false, _) => "bg-gray-200 "
        },
      ]->Array.join(" ")}>
      {cardId->React.string}
    </div>
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
  | BasePile(int)
  | BaseFoundation(int)
  | ChildPile(int, int)
  | ChildFoundation(int, int)

let encodeDropId = (d: dropLoc) => {
  switch d {
  | BasePile(num) => ["basepile", num->Int.toString]
  | BaseFoundation(num) => ["basefoundation", num->Int.toString]
  | ChildPile(num, index) => ["childpile", num->Int.toString, index->Int.toString]
  | ChildFoundation(num, index) => ["childfoundation", num->Int.toString, index->Int.toString]
  }->Array.join("-")
}

let decodeDropId = d => {
  let split = d->String.split("-")

  switch split {
  | ["basepile", num] =>
    switch num->Int.fromString {
    | Some(n) => BasePile(n)->Some
    | _ => None
    }

  | ["basefoundation", num] =>
    switch num->Int.fromString {
    | Some(n) => BaseFoundation(n)->Some
    | _ => None
    }

  | ["childpile", num, index] =>
    switch (num->Int.fromString, index->Int.fromString) {
    | (Some(n), Some(i)) => ChildPile(n, i)->Some
    | _ => None
    }

  | ["childfoundation", num, index] =>
    switch (num->Int.fromString, index->Int.fromString) {
    | (Some(n), Some(i)) => ChildFoundation(n, i)->Some
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

    let {isDragging, setNodeRef, listeners, transform} = useDraggable({
      id: card->Card.id,
    })

    let style =
      transform
      ->Nullable.toOption
      ->Option.mapOr(({}: JsxDOMStyle.t), t => {
        transform: `translate3d(${t["x"]->Int.toString}px, ${t["y"]->Int.toString}px, 0)`,
      })

    let reveal = _ => ()

    let canDrop = item => canPutCardOnCard(item, card, hasOnTop)

    let onClick = () => {
      if !card.revealed && !hasOnTop {
        reveal(card)
      }
    }

    <div
      ref={ReactDOM.Ref.callbackDomRef(setNodeRef)}
      onPointerDown={listeners["onPointerDown"]}
      style={style}>
      <div
        className={[
          "border border-gray-300 rounded h-[80px] w-[57px] -mb-[58px] bg-white shadow-sm px-1 leading-none py-0.5",
          switch card.suit {
          | Spades => "text-black"
          | Hearts => "text-red-600"
          | Diamonds => "text-red-600"
          | Clubs => "text-black"
          },
        ]->Array.join(" ")}>
        {card->Card.string}
      </div>
      {hasOnTop
        ? <CardComp aligned stack canPutCardOnCard index={index + 1} num place />
        : <DropZone
            canDrop={true}
            cardId={encodeDropId(
              switch place {
              | Pile => ChildPile(num, index)
              | Foundation => ChildFoundation(num, index)
              },
            )}
          />}
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
        : <DropZone canDrop={true} empty={true} cardId={encodeDropId(BasePile(num))} />}
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
        : <DropZone canDrop={true} empty={true} cardId={encodeDropId(BaseFoundation(num))} />}
    </div>
  }
}

@react.component
let make = () => {
  let (game, setGame) = React.useState(() => Klondike.initiateGame())
  let (movingCard, setMovingCard) = React.useState(() => None)

  let {
    piles: (p0, p1, p2, p3, p4, p5, p6),
    foundations: (f0, f1, f2, f3),
    movesCounter,
    gameEnded,
  } = game

  let restart = _ => ()

  let onDragStart = React.useCallback0((dragStartEvent: dragStartEvent) => {
    Js.log2("onDragStart: ", dragStartEvent)
    setMovingCard(_ => Some(dragStartEvent["active"]["id"]))
  })

  let onDragCancel = React.useCallback0(() => {
    setMovingCard(_ => None)
  })
  let onDragEnd = React.useCallback0((dragEndEvent: dragEndEvent) => {
    Console.log(dragEndEvent["over"]["id"])
    Console.log(movingCard)

    setMovingCard(_ => None)
  })

  <DndContext onDragStart={onDragStart} onDragEnd={onDragEnd} onDragCancel={onDragCancel}>
    <div className="p-6">
      <div>
        <button onClick={restart}> {"restart"->React.string} </button>
        <div> {("Moves: " ++ movesCounter->Int.toString)->React.string} </div>
        <div> {gameEnded ? "You win!"->React.string : React.null} </div>
      </div>
      <div className={"flex flex-row gap-2 py-1"}>
        <Foundation stack={f0} num={0} />
        <Foundation stack={f1} num={1} />
        <Foundation stack={f2} num={2} />
        <Foundation stack={f3} num={3} />
      </div>
      <div className={"flex flex-row gap-2 py-1"}>
        <Pile stack={p0} num={0} />
        <Pile stack={p1} num={1} />
        <Pile stack={p2} num={2} />
        <Pile stack={p3} num={3} />
        <Pile stack={p4} num={4} />
        <Pile stack={p5} num={5} />
        <Pile stack={p6} num={6} />
      </div>
    </div>
  </DndContext>
}

// let {isDragging, setNodeRef} = useDraggable({
//   id: "test",
// })

//  <div className="bg-gray-300">
//       <DraggableTest />
//       <div ref={ReactDOM.Ref.callbackDomRef(setNodeRef)} className={"bg-blue-300 h-20 w-20"}>
//         {"Test"->React.string}
//       </div>
//     </div>
