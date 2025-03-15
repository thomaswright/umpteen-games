@decco
type rank =
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | R16
  | R17
  | R18
  | R19
  | R20
  | R21

@decco
type card = {rank: rank}

let allRanks = [
  R0,
  R1,
  R2,
  R3,
  R4,
  R5,
  R6,
  R7,
  R8,
  R9,
  R10,
  R11,
  R12,
  R13,
  R14,
  R15,
  R16,
  R17,
  R18,
  R19,
  R20,
  R21,
]

let equals = (a, b) => {
  a.rank == b.rank
}

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

let toString = card => {
  card->card_encode->Js.Json.stringify
}

let displayRank = card =>
  switch card.rank {
  | R0 => "0"
  | R1 => "1"
  | R2 => "2"
  | R3 => "3"
  | R4 => "4"
  | R5 => "5"
  | R6 => "6"
  | R7 => "7"
  | R8 => "8"
  | R9 => "9"
  | R10 => "10"
  | R11 => "11"
  | R12 => "12"
  | R13 => "13"
  | R14 => "14"
  | R15 => "15"
  | R16 => "16"
  | R17 => "17"
  | R18 => "18"
  | R19 => "19"
  | R20 => "20"
  | R21 => "21"
  }

let rotation = (card: card) => {
  let rankJitter = mod(allRanks->Array.findIndex(r => r == card.rank), 4) - 2

  `rotate(${rankJitter->Int.toString}deg)`
}

module Display = {
  @react.component
  let make = (~card, ~id, ~cardRef, ~onMouseDown) => {
    <div id={id} ref={cardRef} onMouseDown={onMouseDown} className="absolute w-14 h-20 select-none">
      <div
        style={{
          // transform: rotation(card),
          // position: "relative",
          color: "white",
        }}
        className={[
          " border border-gray-300 rounded w-14 h-20 bg-black shadow-sm px-1 leading-none py-0.5 cursor-default",
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
          </span>
        </span>
      </div>
    </div>
  }
}

let getShuffledDeck = () => {
  allRanks
  ->Array.reduce([], (a, rank) => {
    a->Array.concat([
      (
        {
          rank: rank,
        }: card
      ),
    ])
  })
  ->Array.toShuffled
}
