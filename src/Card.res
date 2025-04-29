@scope("classList") @send external classListAdd: (Webapi.Dom.Element.t, string) => unit = "add"
@scope("classList") @send
external classListRemove: (Webapi.Dom.Element.t, string) => unit = "remove"

@decco
type suit =
  | Spades
  | Hearts
  | Diamonds
  | Clubs

@decco
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

@decco
type card = {
  suit: suit,
  rank: rank,
  deck: int,
}

@decco
type sides = {
  card: card,
  hidden: bool,
}

type color = Black | Red

let allRanks = [RA, R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK]

let allSuits = [Spades, Hearts, Diamonds, Clubs]

let equals = (a, b) => {
  a.suit == b.suit && a.rank == b.rank
}
let isRed = (v: sides) => v.card.suit == Hearts || v.card.suit == Diamonds
let isBlack = (v: sides) => v.card.suit == Spades || v.card.suit == Clubs

let rankIsAbove = (a, b) => {
  allRanks->Array.findIndex(x => x == a.card.rank) ==
    allRanks->Array.findIndex(x => x == b.card.rank) + 1
}

let rankIsAboveCyclic = (a, b) => {
  if a.card.rank == RA {
    b.card.rank == RK
  } else {
    allRanks->Array.findIndex(x => x == a.card.rank) ==
      allRanks->Array.findIndex(x => x == b.card.rank) + 1
  }
}

let rankIsAdjacent = (a, b) => {
  rankIsAbove(b, a) || rankIsAbove(a, b)
}

let rankString = v => (v.card.rank :> string)

let stringToRank = s => (s :> rank)

let suitString = v => (v.card.suit :> string)

let stringToSuit = s => (s :> suit)

let displayRank = v =>
  switch v.card.rank {
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

let displaySuit = v =>
  switch v.card.suit {
  | Spades => "♠"
  | Hearts => "♥"
  | Diamonds => "♦"
  | Clubs => "♣"
  }

let color = v =>
  switch v.card.suit {
  | Spades => Black
  | Hearts => Red
  | Diamonds => Red
  | Clubs => Black
  }

let colorHex = v =>
  switch v.card.suit {
  | Spades => "hsl(0 0% 0%)"
  | Hearts => "hsl(0 100% 44.31%)"
  | Diamonds => "hsl(0 100% 44.31%)"
  | Clubs => "hsl(0 0% 0%)"
  }

let multiColorHex = v =>
  switch v.card.suit {
  | Spades => "hsl(224 100% 40%)"
  | Hearts => "hsl(0 100% 43%)"
  | Diamonds => "hsl(39 100% 50%)"
  | Clubs => "hsl(130 100% 25%)"
  }

let multiColorPastel = v =>
  switch v.card.suit {
  | Spades => "hsl(224 100% 97%)"
  | Hearts => "hsl(0 100% 97%)"
  | Diamonds => "hsl(48 100% 97%)"
  | Clubs => "hsl(130 100% 97%)"
  }

let isOppositeColor = (a, b) => isRed(a) != isRed(b)

let rotation = (card: card) => {
  let suitJitter = allSuits->Array.findIndex(s => s == card.suit) - 2
  let rankJitter = mod(allRanks->Array.findIndex(r => r == card.rank), 4) - 2

  `rotate(${(suitJitter + rankJitter)->Int.toString}deg)`
}

let toString = card => {
  card->card_encode->Js.Json.stringify
}

module Display = {
  @react.component
  let make = (~card, ~id, ~cardRef, ~onMouseDown, ~multiColor=false) => {
    <div id={id} ref={cardRef} onMouseDown={onMouseDown} className="absolute w-14 h-20 select-none">
      <div
        style={{
          transform: rotation(card.card),
          // position: "relative",
          color: multiColor ? card->multiColorHex : card->colorHex,
        }}
        className={[
          "relative rounded w-14 h-20  shadow-sm leading-none  cursor-default overflow-hidden",
        ]->Array.join(" ")}>
        <div
          className={[
            "absolute bg-red-800 border border-red-950 w-full h-full card-back",
            card.hidden ? "" : "hidden",
          ]->Array.join(" ")}
        />
        <span
          className="flex flex-col py-0.5 px-1 bg-white w-full  h-full border border-gray-400 rounded"
          style={{
            backgroundColor: multiColor ? multiColorPastel(card) : "white",
          }}>
          <span className="flex flex-row">
            <span
              className={[
                "font-medium ",
                switch card.card.rank {
                | R10 => "tracking-[-0.1rem] w-4"
                | _ => "w-3.5"
                },
              ]->Array.join(" ")}>
              {card->displayRank->React.string}
            </span>
            <span className="w-3.5 flex flex-row justify-center">
              {card->displaySuit->React.string}
            </span>
          </span>
          <span className="w-3.5 flex flex-row mt-0.5 -ml-0.5">
            {card->displaySuit->React.string}
          </span>
        </span>
      </div>
    </div>
  }
}

let hide = element => {
  element
  ->Webapi.Dom.Element.querySelector(".card-back")
  ->Option.mapOr((), cardBackElement => {
    cardBackElement->classListRemove("hidden")
  })
}

let show = element => {
  element
  ->Webapi.Dom.Element.querySelector(".card-back")
  ->Option.mapOr((), cardBackElement => {
    cardBackElement->classListAdd("hidden")
  })
}

let showOrHide = (card: sides, element) => {
  if card.hidden {
    hide(element)
  } else {
    show(element)
  }
}

let showAfter = (a, n) =>
  a->Array.mapWithIndex((v, i) => {
    if i >= n {
      {...v, hidden: false}
    } else {
      v
    }
  })

let hideAfter = (a, n) =>
  a->Array.mapWithIndex((v, i) => {
    if i >= n {
      {...v, hidden: true}
    } else {
      v
    }
  })

let getOneSuitDeck = (deck, suit, hidden) => {
  allRanks->Array.reduce([], (a, rank) => {
    a->Array.concat([
      {
        card: {
          suit,
          rank,
          deck,
        },
        hidden,
      },
    ])
  })
}

let getDeck = (deck, hidden) => {
  allRanks->Array.reduce([], (a, rank) => {
    allSuits->Array.reduce(a, (a2, suit) => {
      a2->Array.concat([
        {
          card: {
            suit,
            rank,
            deck,
          },
          hidden,
        },
      ])
    })
  })
}
