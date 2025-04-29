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

let decAndAltValidation = (dragPile: array<Card.sides>) => {
  dragPile->pileValidation((onTop, onBottom) =>
    Card.rankIsAbove(onBottom, onTop) && onTop->Card.color != onBottom->Card.color
  )
}

let decValidation = (dragPile: array<Card.sides>) => {
  dragPile->pileValidation((onTop, onBottom) =>
    Card.rankIsAbove(onBottom, onTop) && onTop.card.suit == onBottom.card.suit
  )
}

let decCyclicValidation = (dragPile: array<Card.sides>) => {
  dragPile->pileValidation((onTop, onBottom) =>
    Card.rankIsAboveCyclic(onBottom, onTop) && onTop.card.suit == onBottom.card.suit
  )
}
