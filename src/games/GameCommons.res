let decAndAltValidation = dragPile => {
  let (dragPileIsValid, _) =
    dragPile
    ->Array.toReversed
    ->Array.reduce((true, None), ((isStillValid, onTop), onBottom) => {
      !isStillValid
        ? (false, None)
        : switch (onTop, onBottom) {
          | (Some(onTop), onBottom) => (
              Card.rankIsBelow(onTop, onBottom) && onTop->Card.color != onBottom->Card.color,
              Some(onBottom),
            )
          | _ => (true, Some(onBottom))
          }
    })
  dragPileIsValid
}

let decValidation = dragPile => {
  let (dragPileIsValid, _) =
    dragPile
    ->Array.toReversed
    ->Array.reduce((true, None), ((isStillValid, onTop), onBottom) => {
      !isStillValid
        ? (false, None)
        : switch (onTop, onBottom) {
          | (Some(onTop), onBottom) => (Card.rankIsBelow(onTop, onBottom), Some(onBottom))
          | _ => (true, Some(onBottom))
          }
    })
  dragPileIsValid
}
