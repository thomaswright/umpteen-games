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
}

type color = Black | Red

let allRanks = [RA, R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK]

let allSuits = [Spades, Hearts, Diamonds, Clubs]

let equals = (a, b) => {
  a.suit == b.suit && a.rank == b.rank
}
let isRed = (card: card) => card.suit == Hearts || card.suit == Diamonds
let isBlack = (card: card) => card.suit == Spades || card.suit == Clubs

let rankIsBelow = (a, b) => {
  allRanks->Array.findIndex(x => x == a.rank) == allRanks->Array.findIndex(x => x == b.rank) - 1
}

let rankIsAbove = (a, b) => {
  allRanks->Array.findIndex(x => x == a.rank) == allRanks->Array.findIndex(x => x == b.rank) + 1
}

let rankIsAdjacent = (a, b) => {
  rankIsBelow(a, b) || rankIsAbove(a, b)
}

let rankString = card => (card.rank :> string)

let stringToRank = s => (s :> rank)

let suitString = card => (card.suit :> string)

let stringToSuit = s => (s :> suit)

let displayRank = card =>
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

let displaySuit = card =>
  switch card.suit {
  | Spades => "♠"
  | Hearts => "♥"
  | Diamonds => "♦"
  | Clubs => "♣"
  }

let color = card =>
  switch card.suit {
  | Spades => Black
  | Hearts => Red
  | Diamonds => Red
  | Clubs => Black
  }

let colorHex = card =>
  switch card.suit {
  | Spades => "hsl(0 0% 0%)"
  | Hearts => "hsl(0 100% 44.31%)"
  | Diamonds => "hsl(0 100% 44.31%)"
  | Clubs => "hsl(0 0% 0%)"
  }

let multiColorHex = card =>
  switch card.suit {
  | Spades => "hsl(223.96 100% 40.67%)"
  | Hearts => "hsl(0 100% 42.94%)"
  | Diamonds => "hsl(39.12 100% 50%)"
  | Clubs => "hsl(130.15 100% 30.51%)"
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
          transform: rotation(card),
          // position: "relative",
          color: multiColor ? card->multiColorHex : card->colorHex,
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

let getShuffledDeck = () => {
  allRanks
  ->Array.reduce([], (a, rank) => {
    allSuits->Array.reduce(a, (a2, suit) => {
      a2->Array.concat([
        (
          {
            suit,
            rank,
          }: card
        ),
      ])
    })
  })
  ->Array.toShuffled
}
