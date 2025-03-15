open Webapi.Dom
open Types

@val @module("./other.js")
external condInterval: (unit => unit, int, unit => bool) => unit = "condInterval"
type stateActor = User | Auto

let easeOutQuad = (t: float) => 1. -. (1. -. t) *. (1. -. t)
module type GameRules = {
  type game
  type space
  let getSpace: Element.t => option<space>
  let spaceToString: space => string

  let initiateGame: unit => game
  let getSpaceLocs: game => array<(space, space, pos)>
  let applyMoveToOthers: (space, game, space => unit) => unit
  let canDrag: (space, game) => bool
  let canDrop: (space, space, game) => bool
  let onDrop: (space, space, game) => game

  let autoProgress: ((game => game) => unit) => bool

  module Board: {
    type props<'setRef, 'onMouseDown, 'setGame, 'moveToState, 'autoProgress, 'game, 'undo> = {
      setRef: 'setRef,
      onMouseDown: 'onMouseDown,
      setGame: 'setGame,
      moveToState: 'moveToState,
      autoProgress: 'autoProgress,
      game: 'game,
      undo: 'undo,
    }
    let make: props<
      space => ReactDOM.Ref.callbackDomRef,
      'a,
      (game => game) => unit,
      unit => unit,
      unit => 'b,
      game,
      unit => unit,
    > => React.element
  }

  module AllCards: {
    type props<'setRef, 'onMouseDown> = {
      setRef: 'setRef,
      onMouseDown: 'onMouseDown,
    }
    let make: props<
      space => ReactDOM.Ref.callbackDomRef,
      JsxEventU.Mouse.t => unit,
    > => React.element
  }
}

module GameBase = (GameRules: GameRules) => {
  @set @scope("style") external setStyleLeft: (Dom.element, string) => unit = "left"
  @set @scope("style") external setStyleDisplay: (Dom.element, string) => unit = "display"
  @set @scope("style") external setStyleTop: (Dom.element, string) => unit = "top"
  @set @scope("style") external setStyleZIndex: (Dom.element, string) => unit = "z-index"

  @val @scope("performance") external now: unit => float = "now"
  @val external requestAnimationFrame: (float => unit) => unit = "requestAnimationFrame"

  @val @module("./other.js")
  external appendReactElement: (React.element, string) => unit = "appendReactElement"

  let zIndexFromElement = element => {
    Obj.magic(element)["style"]["z-index"]->Int.fromString
  }

  type historySnapshot = {
    game: GameRules.game,
    actor: stateActor,
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

  type state = {history: array<historySnapshot>}

  type undoStats = {
    currentUndoDepth: int,
    undos: array<int>,
  }

  let undoStats = ref({
    currentUndoDepth: 0,
    undos: [],
  })

  let state = ref({
    history: [{actor: User, game: GameRules.initiateGame()}],
  })

  let listeners = Set.make()

  let subscribe = listener => {
    listeners->Set.add(listener)
    let unsubscribe = () => listeners->Set.delete(listener)->ignore
    unsubscribe
  }

  let setUndoStats = f => {
    undoStats.contents = f(undoStats.contents)
  }

  let setState = f => {
    state.contents = f(state.contents)
  }

  let getGame = () => {
    let snapShot = state.contents.history->Array.getUnsafe(state.contents.history->Array.length - 1)

    snapShot.game
  }

  let setGame = f => {
    if undoStats.contents.currentUndoDepth > 0 {
      setUndoStats(undoStats => {
        currentUndoDepth: 0,
        undos: Array.concat(undoStats.undos, [undoStats.currentUndoDepth]),
      })
    }

    setState(state => {
      let newGame = f(getGame())
      listeners->Set.forEach(listener => listener(_ => newGame))

      {
        history: Array.concat(state.history, [{game: newGame, actor: Auto}]),
      }
    })
  }

  let snapshot = () => {
    setState(state => {
      history: state.history->Common.ArrayAux.update(state.history->Array.length - 1, v => {
        ...v,
        actor: User,
      }),
    })
  }

  let undo = () => {
    if state.contents.history->Array.filter(v => v.actor == User)->Array.length > 1 {
      setState(state => {
        let newHistory = state.history->Common.ArrayAux.sliceBefore(v => v.actor == User)
        {
          history: newHistory,
        }
      })

      setUndoStats(undoStats => {
        ...undoStats,
        currentUndoDepth: undoStats.currentUndoDepth + 1,
      })
    }
  }

  let useGame = () => {
    let (externalState, setExternalState) = React.useState(() => getGame())
    React.useEffect0(() => {
      let unsubscribe = subscribe(setExternalState)
      Some(() => unsubscribe())
    })
    let game = externalState

    game
  }

  module BoardWrapper = {
    @react.component
    let make = (~setRef, ~onMouseDown, ~setGame, ~moveToState, ~autoProgress, ~undo) => {
      let game = useGame()
      let undo = () => {
        undo()
        moveToState()
      }
      <GameRules.Board setRef onMouseDown setGame moveToState autoProgress game undo />
    }
  }

  @react.component
  let make = () => {
    let refs = React.useRef([])

    let dragCard: React.ref<option<Dom.element>> = React.useRef(None)
    let offset = React.useRef((0, 0))
    let originalData = React.useRef(None)

    let getElement = a => refs.current->Array.find(el => el->GameRules.getSpace == Some(a))

    let setRef = card => (element: Js.Nullable.t<Dom.element>) => {
      switch element {
      | Value(a) => {
          a->Element.setId(card->GameRules.spaceToString)

          refs.current->Array.push(a)
        }
      | Null => ()
      | Undefined => ()
      }
    }

    let applyMoveToOthers = (element, f) => {
      element
      ->GameRules.getSpace
      ->Option.mapOr((), space => {
        let appliedF = s => {
          s
          ->getElement
          ->Option.mapOr((), childEl => {
            f(childEl)
          })
        }

        GameRules.applyMoveToOthers(space, getGame(), appliedF)
      })
    }

    let rec liftUp = (element, zIndex) => {
      element->setStyleZIndex(zIndex->Int.toString)
      applyMoveToOthers(element, childEl => {
        liftUp(childEl, zIndex + 1)
      })
    }

    let rec setDown = (element, zIndex) => {
      zIndex->Option.mapOr((), zIndex => {
        element->setStyleZIndex(zIndex->Int.toString)
      })

      applyMoveToOthers(element, childEl =>
        setDown(childEl, zIndex->Option.map(zIndex => zIndex + 1))
      )
    }

    let rec move = (element, left, top, offset) => {
      element->setStyleLeft(left->Int.toString ++ "px")
      element->setStyleTop(top->Int.toString ++ "px")

      offset->Option.mapOr((), ((leftOffset, topOffset)) => {
        applyMoveToOthers(element, childEl =>
          move(childEl, left + leftOffset, top + topOffset, offset)
        )
      })
    }

    let moveWithTime = (element, refPos, targetLeft, targetTop, targetZIndex, offset, duration) => {
      let start = element->elementPosition

      let startZIndex = element->zIndexFromElement

      let boardPos =
        document
        ->Document.getElementById("board")
        ->Option.mapOr(
          {
            top: 0.,
            left: 0.,
            bottom: 0.,
            right: 0.,
          },
          board => board->elementPosition,
        )

      let start = {
        top: start.top -. boardPos.top,
        left: start.left -. boardPos.left,
        bottom: start.bottom -. boardPos.bottom,
        right: start.right -. boardPos.right,
      }

      let adjustedTargetLeft = targetLeft +. refPos.left -. boardPos.left
      let adjustedTargetTop = targetTop +. refPos.top -. boardPos.top

      let startTime = now()

      let rec step: float => unit = currentTime => {
        let elapsedTime = currentTime -. startTime
        let progress = Math.min(elapsedTime /. duration, 1.) // Clamp progress between 0 and 1
        let easedProgress = easeOutQuad(progress)
        // let easedProgress = progress
        let leftMove = start.left +. (adjustedTargetLeft -. start.left) *. easedProgress
        let topMove = start.top +. (adjustedTargetTop -. start.top) *. easedProgress

        move(element, leftMove->Int.fromFloat, topMove->Int.fromFloat, offset)

        if progress < 1. {
          requestAnimationFrame(step)
        } else {
          // set down
          setDown(element, targetZIndex)
        }
      }

      if (
        start.left != Math.floor(adjustedTargetLeft) ||
        start.top != Math.floor(adjustedTargetTop) ||
        startZIndex != targetZIndex
      ) {
        liftUp(element, 1000 + targetZIndex->Option.getOr(0))
        requestAnimationFrame(step)
      }
    }

    let moveToState = () => {
      GameRules.getSpaceLocs(getGame())->Array.forEach(((space, refSpace, pos)) => {
        switch (getElement(space), getElement(refSpace)) {
        | (Some(element), Some(refElement)) =>
          let refPos = refElement->elementPosition
          moveWithTime(
            element,
            refPos,
            pos.x->Int.toFloat,
            pos.y->Int.toFloat,
            Some(pos.z),
            None,
            300.,
          )
        | _ => ()
        }
      })
    }

    let getOverlap = (aEl, bEl) => {
      let aPos = aEl->elementPosition
      let bPos = bEl->elementPosition

      let overlapX = Math.max(
        0.,
        Math.min(aPos.right, bPos.right) -. Math.max(aPos.left, bPos.left),
      )
      let overlapY = Math.max(
        0.,
        Math.min(aPos.bottom, bPos.bottom) -. Math.max(aPos.top, bPos.top),
      )

      overlapX *. overlapY
    }

    let onMouseDown = event => {
      let eventElement =
        event
        ->JsxEvent.Mouse.currentTarget
        ->Obj.magic

      switch eventElement->GameRules.getSpace {
      | Some(space) =>
        if GameRules.canDrag(space, getGame()) {
          dragCard.current = eventElement->Some

          let dragCardPos = eventElement->elementPosition

          let boardPos =
            document
            ->Document.getElementById("board")
            ->Option.mapOr(
              {
                top: 0.,
                left: 0.,
                bottom: 0.,
                right: 0.,
              },
              board => board->elementPosition,
            )

          originalData.current = eventElement->zIndexFromElement->Option.map(v => (dragCardPos, v))

          liftUp(eventElement, 1000)

          let pos = event->eventPosition

          offset.current = (
            event->JsxEvent.Mouse.clientX - pos.left->Int.fromFloat + boardPos.left->Int.fromFloat,
            event->JsxEvent.Mouse.clientY - pos.top->Int.fromFloat + boardPos.top->Int.fromFloat,
          )
        }
      | _ => ()
      }
    }

    let onMouseMove = event => {
      dragCard.current->Option.mapOr((), dragCard => {
        let (offsetX, offsetY) = offset.current
        let leftMove = event->MouseEvent.clientX - offsetX
        let topMove = event->MouseEvent.clientY - offsetY

        move(dragCard, leftMove, topMove, Some(0, 20))
      })
    }

    let getDragCard = () => {
      switch dragCard.current {
      | Some(dragCardEl) =>
        switch dragCardEl->GameRules.getSpace {
        | Some(dragSpace) => Some(dragCardEl, dragSpace)
        | _ => None
        }
      | _ => None
      }
    }

    let autoProgress = () => {
      condInterval(
        () => {
          moveToState()
        },
        300,
        () => {
          GameRules.autoProgress(setGame)
        },
      )
    }

    let onMouseUp = _ => {
      switch getDragCard() {
      | Some((dragCardEl, dragSpace)) => {
          let dropOn =
            refs.current
            ->Array.reduce(None, (acc: option<(float, Dom.element)>, el) => {
              el
              ->GameRules.getSpace
              ->Option.mapOr(acc, elSpace => {
                if GameRules.canDrop(dragSpace, elSpace, getGame()) {
                  let overlap = getOverlap(el, dragCardEl)
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
            })
            ->Option.map(((_, x)) => x)

          switch dropOn {
          | None => ()
          | Some(dropOnEl) =>
            switch dropOnEl->GameRules.getSpace {
            | Some(dropOnSpace) => {
                setGame(game => {
                  GameRules.onDrop(dropOnSpace, dragSpace, game)
                })
                snapshot()
              }
            | None => ()
            }
          }

          moveToState()

          autoProgress()
        }
      | None => ()
      }

      dragCard.current = None
    }

    React.useEffect(() => {
      window->Window.addMouseMoveEventListener(onMouseMove)
      window->Window.addMouseUpEventListener(onMouseUp)
      moveToState()
      autoProgress()
      None
    }, [])

    <div id={"board"} className="relative m-5">
      <BoardWrapper onMouseDown setRef setGame moveToState autoProgress undo />
      <GameRules.AllCards onMouseDown setRef />
    </div>
  }
}
