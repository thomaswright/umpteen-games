@@warning("-44")
open Webapi.Dom

@val @module("./other.js")
external condInterval: (unit => unit, int, unit => bool) => unit = "condInterval"

let easeOutQuad = (t: float) => 1. -. (1. -. t) *. (1. -. t)

type pos = {
  x: int,
  y: int,
  z: int,
}

type stateActor = User | Auto

type autoProgress<'a> = Send('a) | Seek | DoNothing

type droppedUpon<'game, 'dragPile> = ('game, 'dragPile) => option<'game>

type movableSpace<'game, 'space, 'dragPile> = {
  locationAdjustment: pos,
  baseSpace: 'space,
  dragPile: unit => option<'dragPile>,
  autoProgress: unit => autoProgress<'dragPile>,
  droppedUpon: droppedUpon<'game, 'dragPile>,
  applyMoveToOthers: ('space => unit) => unit,
}

type staticSpace<'game, 'dragPile> = {
  droppedUpon: droppedUpon<'game, 'dragPile>,
  autoProgress: bool,
}

type spaceFunction<'game, 'space, 'dragPile> =
  Movable(movableSpace<'game, 'space, 'dragPile>) | Static(staticSpace<'game, 'dragPile>)

type getRule<'game, 'space, 'dragPile> = (
  'game,
  'space,
) => option<spaceFunction<'game, 'space, 'dragPile>>

module type GameRules = {
  type game
  type space
  type dragPile

  let getSpace: Element.t => option<space>
  let spaceToString: space => string
  let initiateGame: unit => game
  let getRule: getRule<game, space, dragPile>
  let removeDragFromGame: (game, dragPile) => game
  let winCheck: game => bool

  module Board: {
    type props<
      'setRef,
      'onMouseDown,
      'setGame,
      'moveToState,
      'autoProgress,
      'game,
      'undo,
      'isWin,
    > = {
      setRef: 'setRef,
      onMouseDown: 'onMouseDown,
      setGame: 'setGame,
      moveToState: 'moveToState,
      autoProgress: 'autoProgress,
      game: 'game,
      undo: 'undo,
      isWin: 'isWin,
    }
    let make: props<
      space => ReactDOM.Ref.callbackDomRef,
      'a,
      (game => game) => unit,
      unit => unit,
      unit => 'b,
      game,
      unit => unit,
      bool,
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

module Create = (GameRules: GameRules) => {
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
      let isWin = GameRules.winCheck(game)

      <GameRules.Board setRef onMouseDown setGame moveToState autoProgress game undo isWin />
    }
  }

  type dragData = {
    dragElement: Dom.element,
    offset: (int, int),
    dragSpace: GameRules.space,
    dragPile: GameRules.dragPile,
  }

  @react.component
  let make = () => {
    let refs = React.useRef([])

    let dragData: React.ref<option<dragData>> = React.useRef(None)

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

        GameRules.getRule(getGame(), space)->Option.mapOr((), rule => {
          switch rule {
          | Static(_) => ()
          | Movable({applyMoveToOthers}) => applyMoveToOthers(appliedF)
          }
        })
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
      refs.current->Array.forEach(element => {
        element
        ->GameRules.getSpace
        ->Option.flatMap(space => GameRules.getRule(getGame(), space))
        ->Option.mapOr((), rule => {
          switch rule {
          | Static(_) => ()
          | Movable({locationAdjustment, baseSpace}) =>
            baseSpace
            ->getElement
            ->Option.mapOr(
              (),
              baseElement => {
                let basePos = baseElement->elementPosition
                moveWithTime(
                  element,
                  basePos,
                  locationAdjustment.x->Int.toFloat,
                  locationAdjustment.y->Int.toFloat,
                  Some(locationAdjustment.z),
                  None,
                  300.,
                )
              },
            )
          }
        })
      })
    }

    let getBoardPos = () => {
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
      let dragElement =
        event
        ->JsxEvent.Mouse.currentTarget
        ->Obj.magic

      dragElement
      ->GameRules.getSpace
      ->Option.mapOr((), dragSpace => {
        GameRules.getRule(getGame(), dragSpace)->Option.mapOr((), rule => {
          switch rule {
          | Static(_) => ()
          | Movable({dragPile}) =>
            dragPile()->Option.mapOr(
              (),
              dragPile => {
                let boardPos = getBoardPos()
                let eventPos = event->eventPosition

                dragData.current = Some({
                  dragSpace,
                  dragPile,
                  dragElement,
                  offset: (
                    event->JsxEvent.Mouse.clientX -
                    eventPos.left->Int.fromFloat +
                    boardPos.left->Int.fromFloat,
                    event->JsxEvent.Mouse.clientY -
                    eventPos.top->Int.fromFloat +
                    boardPos.top->Int.fromFloat,
                  ),
                })
                liftUp(dragElement, 1000)
              },
            )
          }
        })
      })
    }

    let onMouseMove = event => {
      dragData.current->Option.mapOr((), dragData => {
        let (offsetX, offsetY) = dragData.offset
        let leftMove = event->MouseEvent.clientX - offsetX
        let topMove = event->MouseEvent.clientY - offsetY

        move(dragData.dragElement, leftMove, topMove, Some(0, 20))
      })
    }

    let autoProgress = () => {
      condInterval(
        () => {
          moveToState()
        },
        300,
        () => {
          let dragPiles = refs.current->Array.filterMap(el => {
            el
            ->GameRules.getSpace
            ->Option.flatMap(elSpace => GameRules.getRule(getGame(), elSpace))
            ->Option.mapOr(None, rule => {
              switch rule {
              | Movable({autoProgress}) =>
                switch autoProgress() {
                | Send(dragPile) => Some(dragPile)
                | _ => None
                }
              | _ => None
              }
            })
          })

          let droppedUpons = refs.current->Array.filterMap(el => {
            el
            ->GameRules.getSpace
            ->Option.flatMap(elSpace => GameRules.getRule(getGame(), elSpace))
            ->Option.mapOr(None, rule => {
              switch rule {
              | Static({autoProgress, droppedUpon}) if autoProgress => Some(droppedUpon)
              | Movable({autoProgress, droppedUpon}) =>
                switch autoProgress() {
                | Seek => Some(droppedUpon)
                | _ => None
                }
              | _ => None
              }
            })
          })

          let op = ref(None)

          dragPiles->Array.forEach(dragPile => {
            droppedUpons->Array.forEach(droppedUpon => {
              if op.contents->Option.isNone {
                op := droppedUpon(getGame()->GameRules.removeDragFromGame(dragPile), dragPile)
              }
            })
          })

          switch op.contents {
          | Some(game) => {
              setGame(_ => game)
              true
            }
          | None => false
          }
        },
      )
    }

    let onMouseUp = _ => {
      switch dragData.current {
      | Some({dragElement, dragPile}) => {
          let greatestOverlap = ref(0.)
          let updatedGame = ref(None)

          refs.current->Array.forEach(el => {
            el
            ->GameRules.getSpace
            ->Option.flatMap(elSpace => GameRules.getRule(getGame(), elSpace))
            ->Option.flatMap(rule => {
              let droppedUpon = switch rule {
              | Static({droppedUpon}) => droppedUpon
              | Movable({droppedUpon}) => droppedUpon
              }

              droppedUpon(getGame()->GameRules.removeDragFromGame(dragPile), dragPile)
            })
            ->Option.mapOr((), newGame => {
              let overlap = getOverlap(el, dragElement)
              if overlap > greatestOverlap.contents {
                greatestOverlap := overlap
                updatedGame := Some(newGame)
              }
            })
          })

          updatedGame.contents->Option.mapOr((), updatedGame => {
            setGame(_ => updatedGame)
            snapshot()
          })
          moveToState()
          autoProgress()
        }
      | None => ()
      }

      dragData.current = None
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
