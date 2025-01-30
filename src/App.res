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

  let string = (card: card) => {
    let rankString = switch card.rank {
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

    let suitString = switch card.suit {
    | Spades => "♠"
    | Hearts => "♥"
    | Diamonds => "♦"
    | Clubs => "♣"
    }

    rankString ++ suitString
  }

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

  let canPutOnPile = (a: Card.card, b: Card.card) => {
    a.revealed && b.revealed && Card.rankIsAbove(a, b) && Card.isOppositeColor(a, b)
  }

  let canPutOnFoundation = (a: Card.card, b: Card.card) => {
    a.cardOnTop->Option.isNone &&
    a.revealed &&
    b.revealed &&
    Card.rankIsBelow(a, b) &&
    a.suit == b.suit
  }

  let initiateGame = () => {
    let shuffledDeck = getShuffledDeck()

    {
      piles: (
        [],
        shuffledDeck->Array.slice(~start=0, ~end=1),
        shuffledDeck->Array.slice(~start=1, ~end=3),
        shuffledDeck->Array.slice(~start=3, ~end=6),
        shuffledDeck->Array.slice(~start=6, ~end=10),
        shuffledDeck->Array.slice(~start=10, ~end=15),
        shuffledDeck->Array.slice(~start=15, ~end=21),
      ),
      foundations: ([], [], [], []),
      stock: shuffledDeck->Array.sliceToEnd(~start=21),
      waste: [],
      movesCounter: 0,
      gameEnded: false,
    }
  }
}

type cardType

type monitor = {
  isDragging: unit => bool,
  isOver: unit => bool,
  canDrop: unit => bool,
  getClientOffset: unit => bool,
}

type useDropRecord<'a> = {
  canDrop: Card.card => bool,
  drop: Card.card => unit,
  accept: string,
  collect: monitor => 'a,
}

type useDrop<'a, 'b, 'c> = (unit => useDropRecord<'a>, ('b, 'c)) => ('a, unit => unit)

type dropZoneProps = {
  canDrop: bool,
  isOver: bool,
  beingDragged: bool,
}

@module("react-dnd") external useDrop: useDrop<dropZoneProps, 'b, 'c> = "useDrop"

type useDragRecord<'a> = {
  type_: string,
  item: Card.card,
  canDrag: unit => bool,
  collect: monitor => 'a,
}
type useDrag<'a, 'b> = (unit => useDragRecord<'a>, 'b) => ('a, unit => unit)

type cardCompProps = {isDragging: bool}

@module("react-dnd") external useDrag: useDrag<cardCompProps, 'b> = "useDrag"

module DropZone = {
  @react.component
  let make = (~onDrop: Card.card => unit, ~canDrop: Card.card => bool) => {
    let ({canDrop, isOver, beingDragged}, drop) = useDrop(() => {
      canDrop,
      drop: onDrop,
      accept: "CARD",
      collect: monitor => {
        beingDragged: monitor.getClientOffset(),
        isOver: monitor.isOver(),
        canDrop: monitor.canDrop(),
      },
    }, (canDrop, onDrop))

    <div
      className={[
        "rounded w-12 h-20",
        switch (isOver, canDrop) {
        | (true, true) => "bg-blue-800"
        | (true, false) => "bg-red-800"
        | (false, _) => "bg-green-800"
        },
      ]->Array.join(" ")}
    />
  }
}

module CardComp = {
  @react.component
  let rec make = (~card: Card.card) => {
    let ({isDragging}, drag) = useDrag(() => {
      type_: "CARD",
      item: card,
      canDrag: () => card.revealed,
      collect: monitor => {
        isDragging: monitor.isDragging(),
      },
    }, [card])

    let onDrop = item => move(item, card)

    let canDrop = item => canPutOnPile(item, card)

    let onClick = () => {
      if !card.revealed && !card.cardOnTop {
        reveal(card)
      }
    }

    <div>
      <div> {card->Card.string->React.string} </div>
      {card.cardOnTop->Option.isSome ? <CardComp /> : <DropZone />}
    </div>
  }
}

module Pile = {
  @react.component
  let make = (~num, ~card: option<Card.card>) => {
    let onDrop = () => ()
    <div>
      {card->Option.isSome
        ? <CardComp stacked={false} card canPutCardOnCard={Klondike.canPutOnPile} />
        : <DropZone onDrop canDrop={card => card.rank == RK} />}
    </div>
  }
}

module Foundation = {
  @react.component
  let make = (~num, ~card: option<Card.card>) => {
    let onDrop = () => ()
    <div>
      {card->Option.isSome
        ? <CardComp stacked={true} card canPutCardOnCard={Klondike.canPutOnFoundation} />
        : <DropZone onDrop canDrop={card => card.rank == RA} />}
    </div>
  }
}

@react.component
let make = () => {
  let (game, setGame) = React.useState(() => initiateGame())

  let restart = _ => ()

  <div className="p-6">
    <div>
      <button onClick={restart}> {"restart"->React.string} </button>
      <div> {("Moves: " ++ movesCounter->Int.toString)->React.string} </div>
      <div> {gameEnded ? "You win!"->React.string : React.null} </div>
    </div>
    <div />
    <div />
  </div>
}
