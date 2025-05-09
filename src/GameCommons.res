let pileValidation = (dragPile: array<Card.sides>, f) => {
  let (dragPileIsValid, _) =
    dragPile
    ->Array.toReversed
    ->Array.reduce((true, None), ((isStillValid, onTop), onBottom) => {
      !isStillValid
        ? (false, None)
        : switch (onTop, onBottom) {
          | (Some(onTop), onBottom) => (f(onBottom, onTop), Some(onBottom))
          | _ => (true, Some(onBottom))
          }
    })
  dragPileIsValid
}

let decAltColorValidation = (dragPile: array<Card.sides>) => {
  dragPile->pileValidation((onBottom, onTop) =>
    Card.rankIsAbove(onBottom, onTop) && onTop->Card.color != onBottom->Card.color
  )
}

let decOneSuitValidation = (dragPile: array<Card.sides>) => {
  dragPile->pileValidation((onBottom, onTop) =>
    Card.rankIsAbove(onBottom, onTop) && onTop.card.suit == onBottom.card.suit
  )
}

let decCyclicOneSuitValidation = (dragPile: array<Card.sides>) => {
  dragPile->pileValidation((onBottom, onTop) => {
    Card.rankIsAboveCyclic(onBottom, onTop) && onTop.card.suit == onBottom.card.suit
  })
}

let decCyclicAnySuitValidation = (dragPile: array<Card.sides>) => {
  dragPile->pileValidation((onBottom, onTop) => {
    Card.rankIsAboveCyclic(onBottom, onTop)
  })
}

let decCyclicSameColorValidation = (dragPile: array<Card.sides>) => {
  dragPile->pileValidation((onBottom, onTop) => {
    Card.rankIsAboveCyclic(onBottom, onTop) && onBottom->Card.color == onTop->Card.color
  })
}

let decCyclicAltColorValidation = (dragPile: array<Card.sides>) => {
  dragPile->pileValidation((onBottom, onTop) => {
    Card.rankIsAboveCyclic(onBottom, onTop) && onBottom->Card.color != onTop->Card.color
  })
}

let flipLastUp = (piles: array<array<Card.sides>>) =>
  piles->Array.map(pile => pile->Common.ArrayAux.updateLast(v => {...v, hidden: false}))
