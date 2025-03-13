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

type color = Black | Red

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

let toString = card => {
  card->card_encode->Js.Json.stringify
}

module Display = {
  @react.component
  let make = (~card, ~id, ~cardRef, ~onMouseDown) => {
    <div id={id} ref={cardRef} onMouseDown={onMouseDown} className="absolute w-14 h-20 select-none">
      <div
        style={{
          transform: rotation(card),
          // position: "relative",
          color: card->colorHex,
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
              {card->rankString->React.string}
            </span>
            <span className="w-3.5 flex flex-row justify-center">
              {card->suitString->React.string}
            </span>
          </span>
          <span className="w-3.5 flex flex-row mt-0.5 -ml-0.5">
            {card->suitString->React.string}
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
