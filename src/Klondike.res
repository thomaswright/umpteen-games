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
type space =
  | Pile(int)
  | Foundation(int)
  | Card(Card.card)

// | CardOnFoundation(int, Card.card)
// | CardOnPile(int, Card.card)
// | CardOnCard(Card.card, Card.card)

@set external setSpace: (Dom.element, string) => unit = "space"
@set external setParent: (Dom.element, string) => unit = "parentSpace"

@set @scope("style") external setStyleLeft: (Dom.element, string) => unit = "left"
@set @scope("style") external setStyleTop: (Dom.element, string) => unit = "top"
@set @scope("style") external setStyleZIndex: (Dom.element, string) => unit = "z-index"

@val @scope("performance") external now: unit => float = "now"
@val external requestAnimationFrame: (float => unit) => unit = "requestAnimationFrame"

let spaceToString = space => {
  space->space_encode->Js.Json.stringify
}

let zIndexFromElement = element => {
  Obj.magic(element)["style"]["z-index"]->Int.fromString
}

let spaceFromElement = element => {
  // element
  // ->Element.getAttribute("space")
  Obj.magic(element)["space"]->Option.flatMap(s => {
    switch s->Js.Json.parseExn->space_decode {
    | Ok(d) => Some(d)
    | _ => None
    }
  })
}
let parentFromElement = element => {
  // element
  // ->Element.getAttribute("space")
  Obj.magic(element)["parentSpace"]->Option.flatMap(s => {
    switch s->Js.Json.parseExn->space_decode {
    | Ok(d) => Some(d)
    | _ => None
    }
  })
}

type position = {
  top: float,
  right: float,
  bottom: float,
  left: float,
}

let elementPosition = element => {
  let a = element->Element.getBoundingClientRect

  {top: a->DomRect.top, right: a->DomRect.right, bottom: a->DomRect.bottom, left: a->DomRect.left}
}

let eventPosition = event => {
  event
  ->JsxEvent.Mouse.currentTarget
  ->Obj.magic
  ->elementPosition
}

@react.component
let make = () => {
  let refs = React.useRef([])
  let setRef = (space, parent) => (element: Js.Nullable.t<Dom.element>) => {
    switch element {
    | Value(a) => {
        a->setSpace(space->spaceToString)
        parent->Option.mapOr((), parent => {
          a->setParent(parent->spaceToString)
        })

        refs.current->Array.push(a)
      }
    | Null => ()
    | Undefined => ()
    }
  }

  let dragCard: React.ref<option<Dom.element>> = React.useRef(None)
  let offset = React.useRef((0, 0))
  let originalData = React.useRef(None)

  let applyToChildren = (element, f) => {
    let elementSpace = element->spaceFromElement

    refs.current->Array.forEach(el => {
      switch (elementSpace, el->parentFromElement) {
      | (Some(Card(elementCard)), Some(Card(parentCard))) =>
        if Card.equals(parentCard, elementCard) {
          f(el)
        }
      | _ => ()
      }
    })
  }

  let rec move = (element, left, top, leftOffset, topOffset, zIndex) => {
    element->setStyleLeft(left->Int.toString ++ "px")
    element->setStyleTop(top->Int.toString ++ "px")
    zIndex->Option.mapOr((), zIndex => {
      element->setStyleZIndex(zIndex->Int.toString)
    })

    applyToChildren(element, childEl =>
      move(
        childEl,
        left + leftOffset,
        top + topOffset,
        leftOffset,
        topOffset,
        zIndex->Option.map(zIndex => zIndex + 1),
      )
    )
  }

  let moveWithTime = (element, targetLeft, targetTop, offsetLeft, offsetTop, zIndex, duration) => {
    let start = element->elementPosition
    let startTime = now()

    let rec step: float => unit = currentTime => {
      let elapsedTime = currentTime -. startTime
      let progress = Math.min(elapsedTime /. duration, 1.) // Clamp progress between 0 and 1
      // let easedProgress = easeOutQuad(progress)
      let easedProgress = progress
      let leftMove = start.left +. (targetLeft -. start.left) *. easedProgress
      let topMove = start.top +. (targetTop -. start.top) *. easedProgress
      move(element, leftMove->Int.fromFloat, topMove->Int.fromFloat, offsetLeft, offsetTop, zIndex)

      if progress < 1. {
        requestAnimationFrame(step)
      }
    }

    requestAnimationFrame(step)
  }

  let rec liftUp = (element, zIndex) => {
    element->setStyleZIndex(zIndex->Int.toString)
    applyToChildren(element, childEl => {
      liftUp(childEl, zIndex + 1)
    })
  }

  let getOverlap = (aEl, bEl) => {
    let aPos = aEl->elementPosition
    let bPos = bEl->elementPosition

    let overlapX = Math.max(0., Math.min(aPos.right, bPos.right) -. Math.max(aPos.left, bPos.left))
    let overlapY = Math.max(0., Math.min(aPos.bottom, bPos.bottom) -. Math.max(aPos.top, bPos.top))

    overlapX *. overlapY
  }

  React.useEffect0(() => {
    window->Window.addMouseMoveEventListener(event => {
      dragCard.current->Option.mapOr(
        (),
        dragCard => {
          let (offsetX, offsetY) = offset.current
          let leftMove = event->MouseEvent.clientX - offsetX
          let topMove = event->MouseEvent.clientY - offsetY

          move(dragCard, leftMove, topMove, 0, 20, None)
        },
      )
    })
    window->Window.addMouseUpEventListener(event => {
      dragCard.current->Option.mapOr(
        (),
        dragCard => {
          let rec buildDragPile = (el, build) => {
            refs.current
            ->Array.find(v => v->parentFromElement == el->spaceFromElement)
            ->Option.mapOr([el], parentEl => buildDragPile(parentEl, [el]))
          }

          let dragPile = buildDragPile(dragCard, [])

          let dropOn =
            refs.current
            ->Array.filter(
              el =>
                dragPile
                ->Array.find(pileEl => pileEl->spaceFromElement == el->spaceFromElement)
                ->Option.isNone,
            )
            ->Array.reduce(
              None,
              (acc, el) => {
                let canDrop =
                  el
                  ->spaceFromElement
                  ->Option.mapOr(
                    false,
                    x => {
                      !(
                        refs.current->Array.some(
                          el2 => {
                            el2->parentFromElement->Option.mapOr(false, p => p == x)
                          },
                        )
                      )
                    },
                  )

                if canDrop {
                  let overlap = getOverlap(el, dragCard)
                  let new = Some((overlap, el))

                  if overlap > 0. {
                    switch acc {
                    | None => new
                    | Some((accOverlap, _)) => accOverlap > overlap ? acc : new
                    }
                  } else {
                    acc
                  }
                } else {
                  acc
                }
              },
            )
            ->Option.map(((_, x)) => x)

          let revert = () => {
            originalData.current->Option.mapOr(
              (),
              ((originalPos, originalZIndex)) => {
                moveWithTime(
                  dragCard,
                  originalPos.left,
                  originalPos.top,
                  0,
                  20,
                  Some(originalZIndex),
                  100.,
                )
              },
            )
          }

          switch dropOn {
          | None => revert()
          | Some(dropOn) => {
              let pos = dropOn->elementPosition
              dropOn
              ->spaceFromElement
              ->Option.mapOr(
                (),
                dropOnSpace => {
                  dragCard->setParent(dropOnSpace->spaceToString)
                },
              )

              let topAdjustment = switch dropOn->spaceFromElement {
              | Some(Card(_card)) => 20.
              | Some(Pile(_num)) => 0.
              | Some(Foundation(_num)) => 0.
              | _ => 0.
              }

              moveWithTime(
                dragCard,
                pos.left,
                pos.top +. topAdjustment,
                0,
                20,
                dropOn->zIndexFromElement->Option.map(v => v + 1),
                100.,
              )
            }
          }
        },
      )

      dragCard.current = None
      originalData.current = None
    })
    None
  })

  <div className="relative">
    {[[], [], [], []]
    ->Array.mapWithIndex((_, i) => {
      <div
        key={Foundation(i)->spaceToString}
        ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i), None))}
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
        key={Pile(i)->spaceToString}
        ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i), None))}
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
        let parent = j == 0 ? Pile(i) : Card(cardsData->Array.getUnsafe(i)->Array.getUnsafe(j - 1))

        <div
          key={Card(card)->spaceToString}
          ref={ReactDOM.Ref.callbackDomRef(setRef(Card(card), Some(parent)))}
          onMouseDown={event => {
            dragCard.current =
              event
              ->JsxEvent.Mouse.currentTarget
              ->Obj.magic
              ->Some

            dragCard.current->Option.mapOr(
              (),
              dragCard => {
                let dragCardPos = dragCard->elementPosition
                originalData.current =
                  dragCard->zIndexFromElement->Option.map(v => (dragCardPos, v))

                liftUp(dragCard, 1000)
              },
            )

            let pos = event->eventPosition

            offset.current = (
              event->JsxEvent.Mouse.clientX - pos.left->Int.fromFloat,
              event->JsxEvent.Mouse.clientY - pos.top->Int.fromFloat,
            )
          }}
          className="absolute w-14 h-20"
          style={{
            top: (100 + j * 20)->Int.toString ++ "px",
            left: (i * 70)->Int.toString ++ "px",
            zIndex: (j + 1)->Int.toString,
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
