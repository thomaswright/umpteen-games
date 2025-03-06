open Webapi.Dom

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

let stockData = shuffledDeck->Array.sliceToEnd(~start=28)

@decco
type space =
  | Stock
  | Waste
  | Pile(int)
  | Foundation(int)
  | Card(Card.card)

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

module CardDisplay = {
  @react.component
  let make = (~card, ~top, ~left, ~zIndex, ~onMouseDown, ~cardRef) => {
    <div
      ref={cardRef}
      onMouseDown={onMouseDown}
      className="absolute w-14 h-20 select-none"
      style={{
        top,
        left,
        zIndex,
      }}>
      <div
        style={{
          // transform: Card.rotation(card),
          // position: "relative",
          // zIndex: ((isDragging ? 100 : 0) + index + 1)->Int.toString,
          color: card->Card.colorHex,
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
  }
}

@react.component
let make = () => {
  let refs = React.useRef([])
  let dragCard: React.ref<option<Dom.element>> = React.useRef(None)
  let offset = React.useRef((0, 0))
  let originalData = React.useRef(None)

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

  let getElement = space => refs.current->Array.find(el => el->spaceFromElement == space)

  let rec baseSpace = el => {
    switch el->parentFromElement {
    | Some(Card(c)) => baseSpace(getElement(Some(Card(c))))
    | Some(x) => Some(x)
    | _ => None
    }
  }

  let rec buildDragPile = (el, build) => {
    refs.current
    ->Array.find(v => v->parentFromElement == el->spaceFromElement)
    ->Option.mapOr([el], parentEl => buildDragPile(parentEl, [el]))
    ->Array.concat(build)
  }

  let canDrag = dragPile => {
    let (dragPileIsValid, _) = dragPile->Array.reduce((true, None), (
      (isStillValid, onTopElement),
      onBottom,
    ) => {
      !isStillValid
        ? (isStillValid, None)
        : switch onTopElement {
          | None => (true, Some(onBottom))
          | Some(onTop) =>
            switch (onTop->spaceFromElement, onBottom->spaceFromElement) {
            | (Some(Card(onTopCard)), Some(Card(onBottomCard))) => (
                Card.rankIsBelow(onTopCard, onBottomCard) &&
                onTopCard->Card.color != onBottomCard->Card.color,
                Some(onBottom),
              )
            | (Some(Card(_onTopCard)), _) => (true, Some(onBottom))
            | _ => (false, None)
            }
          }
    })

    dragPileIsValid
  }

  let canDrop = (dragCard, dropEl) => {
    switch dragCard->spaceFromElement {
    | Some(Card(dragCard)) =>
      switch dropEl->spaceFromElement {
      | Some(Card(dropCard)) =>
        Console.log(baseSpace(Some(dropEl)))
        switch baseSpace(Some(dropEl)) {
        | Some(Foundation(_)) =>
          Card.rankIsBelow(dropCard, dragCard) && dragCard.suit == dropCard.suit
        | Some(Pile(_)) =>
          Card.rankIsAbove(dropCard, dragCard) && dragCard->Card.color != dropCard->Card.color
        | _ => false
        }
      | Some(Foundation(_)) => dragCard.rank == RA
      | Some(Pile(_)) => dragCard.rank == RK
      | _ => false
      }
    | _ => false
    }
  }

  let onMouseDown = event => {
    let eventElement =
      event
      ->JsxEvent.Mouse.currentTarget
      ->Obj.magic

    let dragPile = buildDragPile(eventElement, [])

    let canDrag = canDrag(dragPile)

    if canDrag {
      dragCard.current = eventElement->Some

      let dragCardPos = eventElement->elementPosition

      originalData.current = eventElement->zIndexFromElement->Option.map(v => (dragCardPos, v))

      liftUp(eventElement, 1000)

      let pos = event->eventPosition

      offset.current = (
        event->JsxEvent.Mouse.clientX - pos.left->Int.fromFloat,
        event->JsxEvent.Mouse.clientY - pos.top->Int.fromFloat,
      )
    }
  }

  let onMouseMove = event => {
    dragCard.current->Option.mapOr((), dragCard => {
      let (offsetX, offsetY) = offset.current
      let leftMove = event->MouseEvent.clientX - offsetX
      let topMove = event->MouseEvent.clientY - offsetY

      move(dragCard, leftMove, topMove, 0, 20, None)
    })
  }

  let onMouseUp = _ => {
    dragCard.current->Option.mapOr((), dragCard => {
      let dragPile = buildDragPile(dragCard, [])

      let dropOn =
        refs.current
        ->Array.filter(el =>
          dragPile
          ->Array.find(pileEl => pileEl->spaceFromElement == el->spaceFromElement)
          ->Option.isNone
        )
        ->Array.reduce(None, (acc, el) => {
          let dropHasNoChildren =
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

          let canDrop = dropHasNoChildren && canDrop(dragCard, el)

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
        })
        ->Option.map(((_, x)) => x)

      let revert = () => {
        originalData.current->Option.mapOr((), ((originalPos, originalZIndex)) => {
          moveWithTime(
            dragCard,
            originalPos.left,
            originalPos.top,
            0,
            20,
            Some(originalZIndex),
            100.,
          )
        })
      }

      switch dropOn {
      | None => revert()
      | Some(dropOn) => {
          let pos = dropOn->elementPosition

          dropOn
          ->spaceFromElement
          ->Option.mapOr((), dropOnSpace => {
            dragCard->setParent(dropOnSpace->spaceToString)
          })

          let (topAdjustment, stackAdjustment) = switch dropOn->spaceFromElement {
          | Some(Card(_card)) =>
            switch baseSpace(Some(dropOn)) {
            | Some(Foundation(_)) => (0., 0)
            | _ => (20., 20)
            }
          | Some(Pile(_num)) => (0., 20)
          | Some(Foundation(_num)) => (0., 0)
          | _ => (0., 0)
          }

          moveWithTime(
            dragCard,
            pos.left,
            pos.top +. topAdjustment,
            0,
            stackAdjustment,
            dropOn->zIndexFromElement->Option.map(v => v + 1),
            100.,
          )
        }
      }
    })

    dragCard.current = None
    originalData.current = None
  }

  React.useEffect0(() => {
    window->Window.addMouseMoveEventListener(onMouseMove)
    window->Window.addMouseUpEventListener(onMouseUp)
    None
  })

  let dealToWaste = () => {
    let stock = switch getElement(Some(Stock)) {
    | Some(el) => buildDragPile(el, [])
    | _ => []
    }

    let waste = switch getElement(Some(Waste)) {
    | Some(el) => buildDragPile(el, [])
    | _ => []
    }

    stock
    ->Array.get(0)
    ->Option.mapOr((), topStockEl => {
      switch topStockEl->spaceFromElement {
      | Some(Stock) =>
        let wasteCards =
          waste
          ->Array.toReversed
          ->Array.sliceToEnd(~start=1)

        wasteCards->Array.forEachWithIndex((wasteCard, i) => {
          if i == wasteCards->Array.length - 1 {
            wasteCard->setParent(Stock->spaceToString)
            moveWithTime(wasteCard, 0., 0., 0, 0, Some(1), 200.)
          } else {
            wasteCards
            ->Array.get(i + 1)
            ->Option.flatMap(spaceFromElement)
            ->Option.mapOr(
              (),
              v => {
                wasteCard->setParent(v->spaceToString)
              },
            )
          }
        })

      | Some(_) => {
          let topWasteElement = waste->Array.getUnsafe(0)

          topWasteElement
          ->spaceFromElement
          ->Option.mapOr((), v => {
            topStockEl->setParent(v->spaceToString)
          })

          let pos = topWasteElement->elementPosition

          moveWithTime(
            topStockEl,
            pos.left,
            pos.top,
            0,
            0,
            topWasteElement->zIndexFromElement->Option.map(v => v + 1),
            200.,
          )
        }
      | _ => ()
      }
    })
  }

  <div className="relative">
    <div
      key={Stock->spaceToString}
      ref={ReactDOM.Ref.callbackDomRef(setRef(Stock, None))}
      className="absolute bg-pink-500 rounded w-14 h-20"
      style={{
        top: "0px",
        left: "0px",
        zIndex: "0",
      }}
    />
    {stockData
    ->Array.mapWithIndex((card, i) => {
      let parent = i == 0 ? Stock : Card(stockData->Array.getUnsafe(i - 1))

      <CardDisplay
        card={card}
        key={Card(card)->spaceToString}
        cardRef={ReactDOM.Ref.callbackDomRef(setRef(Card(card), Some(parent)))}
        top={0->Int.toString ++ "px"}
        left={0->Int.toString ++ "px"}
        zIndex={(i + 1)->Int.toString}
        onMouseDown={onMouseDown}
      />
    })
    ->React.array}
    <div
      key={"Stock Cover"}
      // ref={ReactDOM.Ref.callbackDomRef(setRef(Stock, None))}
      onClick={_ => dealToWaste()}
      className="absolute bg-blue-700 rounded w-14 h-20"
      style={{
        top: "0px",
        left: "0px",
        zIndex: "53",
      }}
    />
    <div
      key={Waste->spaceToString}
      ref={ReactDOM.Ref.callbackDomRef(setRef(Waste, None))}
      className="absolute bg-cyan-500 rounded w-14 h-20"
      style={{
        top: "0px",
        left: "70px",
        zIndex: "0",
      }}
    />
    {[[], [], [], []]
    ->Array.mapWithIndex((_, i) => {
      <div
        key={Foundation(i)->spaceToString}
        ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i), None))}
        className="absolute bg-purple-500 rounded w-14 h-20"
        style={{
          top: "100px",
          left: (i * 70)->Int.toString ++ "px",
          zIndex: "0",
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
          top: "200px",
          left: (i * 70)->Int.toString ++ "px",
          zIndex: "0",
        }}
      />
    })
    ->React.array}
    {cardsData
    ->Array.mapWithIndex((cardPile, i) => {
      cardPile
      ->Array.mapWithIndex((card, j) => {
        let parent = j == 0 ? Pile(i) : Card(cardsData->Array.getUnsafe(i)->Array.getUnsafe(j - 1))

        <CardDisplay
          card={card}
          key={Card(card)->spaceToString}
          cardRef={ReactDOM.Ref.callbackDomRef(setRef(Card(card), Some(parent)))}
          top={(200 + j * 20)->Int.toString ++ "px"}
          left={(i * 70)->Int.toString ++ "px"}
          zIndex={(j + 1)->Int.toString}
          onMouseDown={onMouseDown}
        />
      })
      ->React.array
    })
    ->React.array}
  </div>
}
