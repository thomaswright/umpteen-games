open Webapi.Dom

module Card = {
  @decco
  type suit =
    | @as("Spades") Spades
    | @as("Hearts") Hearts
    | @as("Diamonds") Diamonds
    | @as("Clubs") Clubs

  @decco
  type rank =
    | @as("RA") RA
    | @as("R2") R2
    | @as("R3") R3
    | @as("R4") R4
    | @as("R5") R5
    | @as("R6") R6
    | @as("R7") R7
    | @as("R8") R8
    | @as("R9") R9
    | @as("R10") R10
    | @as("RJ") RJ
    | @as("RQ") RQ
    | @as("RK") RK
  @decco
  type card = {
    suit: suit,
    rank: rank,
  }

  let allRanks = [RA, R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK]

  let allSuits = [Spades, Hearts, Diamonds, Clubs]

  let equals = (a, b) => {
    a.suit == b.suit && a.rank == b.rank
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

  let stringToRank = s =>
    switch s {
    | "A" => RA
    | "2" => R2
    | "3" => R3
    | "4" => R4
    | "5" => R5
    | "6" => R6
    | "7" => R7
    | "8" => R8
    | "9" => R9
    | "10" => R10
    | "J" => RJ
    | "Q" => RQ
    | "K" => RK
    | _ => RA
    }

  let stringToSuit = s =>
    switch s {
    | "♠" => Spades
    | "♥" => Hearts
    | "♦" => Diamonds
    | "♣" => Clubs
    | _ => Spades
    }

  let suitString = card =>
    switch card.suit {
    | Spades => "♠"
    | Hearts => "♥"
    | Diamonds => "♦"
    | Clubs => "♣"
    }

  let color = card =>
    switch card.suit {
    | Spades => "hsl(0 0% 0%)"
    | Hearts => "hsl(0 100% 44.31%)"
    | Diamonds => "hsl(0 100% 44.31%)"
    | Clubs => "hsl(0 0% 0%)"
    }

  // let toString = (card: card) => card->rankString ++ "-" ++ card->suitString

  // let fromString = (id: string) => {
  //   {
  //     rank: id->String.split("-")->Array.getUnsafe(0)->stringToRank,
  //     suit: id->String.split("-")->Array.getUnsafe(1)->stringToSuit,
  //   }
  // }

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
            // cardOnTop: None,
          }: Card.card
        ),
      ])
    })
  })
  ->Array.toShuffled
}

let shuffledDeck = getShuffledDeck()

let cardsData = [
  shuffledDeck->Array.slice(~start=0, ~end=1),
  shuffledDeck->Array.slice(~start=1, ~end=3),
  shuffledDeck->Array.slice(~start=3, ~end=6),
  shuffledDeck->Array.slice(~start=6, ~end=10),
  shuffledDeck->Array.slice(~start=10, ~end=15),
  shuffledDeck->Array.slice(~start=15, ~end=21),
  shuffledDeck->Array.slice(~start=21, ~end=28),
]

@decco
type element =
  | Pile(int)
  | Foundation(int)
  | Card(Card.card)

// | CardOnFoundation(int, Card.card)
// | CardOnPile(int, Card.card)
// | CardOnCard(Card.card, Card.card)

@set external setElementData: (Js.nullable<Dom.element>, Js.Json.t) => unit = "elementData"

@set @scope("style") external setStyleLeft: (Dom.element, string) => unit = "left"
@set @scope("style") external setStyleTop: (Dom.element, string) => unit = "top"

@react.component
let make = () => {
  let refs = React.useRef([])
  let setRef = elementType => element => {
    element->setElementData(elementType->element_encode)
    refs.current->Array.push(element)
  }

  let dragCard: React.ref<option<Dom.element>> = React.useRef(None)
  let offset = React.useRef((0, 0))

  let move = (element, left, top, leftOffset, topOffset) => {
    element->setStyleLeft(left->Int.toString ++ "px")
    element->setStyleTop(top->Int.toString ++ "px")

    // refs -> Array.forEach((el) => {
    //   switch el -> Element.getAttribute("parent") -> element_decode {
    //     | CardParent(Card.card) => {

    //     }
    //   }
    //   if (el.elementParent === element.elementId) {
    //     move(el, left + leftOffset, top + topOffset, leftOffset, topOffset);
    //   }
    // });
  }

  React.useEffect0(() => {
    window->Window.addMouseMoveEventListener(event => {
      dragCard.current->Option.mapOr(
        (),
        dragCard => {
          let (offsetX, offsetY) = offset.current
          let leftMove = event->MouseEvent.clientX - offsetX
          let topMove = event->MouseEvent.clientY - offsetY

          move(dragCard, leftMove, topMove, 0, 20)
        },
      )
    })
    None
  })

  <div className="relative">
    {[[], [], [], []]
    ->Array.mapWithIndex((_, i) => {
      <div
        ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
        className="absolute bg-purple-500 rounded w-14 h-20"
        style={{
          top: "0px",
          left: (i * 70)->Int.toString ++ "px",
        }}
      />
    })
    ->React.array}
    {[[], [], [], []]
    ->Array.mapWithIndex((_, i) => {
      <div
        ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
        className="absolute bg-red-500 rounded w-14 h-20"
        style={{
          top: "100px",
          left: (i * 70)->Int.toString ++ "px",
        }}
      />
    })
    ->React.array}
    {cardsData
    ->Array.mapWithIndex((cardPile, i) => {
      cardPile
      ->Array.mapWithIndex((card, j) => {
        <div
          ref={ReactDOM.Ref.callbackDomRef(setRef(Card(card)))}
          onMouseDown={event => {
            dragCard.current =
              event
              ->JsxEvent.Mouse.currentTarget
              ->Obj.magic
              ->Some

            let rect =
              event
              ->JsxEvent.Mouse.currentTarget
              ->Obj.magic
              ->Element.getBoundingClientRect

            offset.current = (
              event->JsxEvent.Mouse.clientX - rect->DomRect.left->Int.fromFloat,
              event->JsxEvent.Mouse.clientY - rect->DomRect.top->Int.fromFloat,
            )
          }}
          className="absolute bg-blue-500 rounded w-14 h-20 border"
          style={{
            top: (100 + j * 20)->Int.toString ++ "px",
            left: (i * 70)->Int.toString ++ "px",
          }}>
          <div
            style={{
              // transform: Card.rotation(card),
              // position: "relative",
              // zIndex: ((isDragging ? 100 : 0) + index + 1)->Int.toString,
              color: card->Card.color,
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
      })
      ->React.array
    })
    ->React.array}
  </div>
}
