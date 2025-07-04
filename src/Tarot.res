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
type card = {rank: rank, deck: int}

@decco
type sides = {
  card: card,
  hidden: bool,
}

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
  a.card.rank == b.card.rank
}

let rankIsAbove = (a, b) => {
  allRanks->Array.findIndex(x => x == a.card.rank) ==
    allRanks->Array.findIndex(x => x == b.card.rank) + 1
}

let rankIsAdjacent = (a, b) => {
  rankIsAbove(b, a) || rankIsAbove(a, b)
}

let rankString = card => (card.card.rank :> string)

let stringToRank = s => (s :> rank)

let toString = card => {
  card->card_encode->Js.Json.stringify
}

let displayRank = card =>
  switch card.card.rank {
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

let rotation = (card: sides) => {
  let rankJitter = mod(allRanks->Array.findIndex(r => r == card.card.rank), 4) - 2

  `rotate(${rankJitter->Int.toString}deg)`
}

module Display = {
  @react.component
  let make = (~card, ~id, ~cardRef, ~onMouseDown) => {
    <div id={id} ref={cardRef} onMouseDown={onMouseDown} className="absolute card-dims select-none">
      <div
        style={{
          // transform: rotation(card),
          // position: "relative",
          backgroundColor: "hsl(250 67% 17%)",
          borderColor: "hsl(250 20% 43%)",
          color: "white",
        }}
        className={[
          " border rounded card-dims  shadow-sm px-1 leading-none py-0.5 cursor-default",
        ]->Array.join(" ")}>
        <span className="flex flex-col text-xl leading-none">
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
          </span>
        </span>
      </div>
    </div>
  }
}

let getDeck = (deck, hidden) => {
  allRanks->Array.reduce([], (a, rank) => {
    a->Array.concat([
      {
        card: {
          rank,
          deck,
        },
        hidden,
      },
    ])
  })
}
