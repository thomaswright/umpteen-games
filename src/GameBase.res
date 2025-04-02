@@warning("-44")
open Webapi.Dom

@scope("classList") @send external classListAdd: (Element.t, string) => unit = "add"
@scope("classList") @send external classListRemove: (Element.t, string) => unit = "remove"

@val @module("./other.js")
external condInterval: (unit => unit, int, unit => bool) => unit = "condInterval"

let easeOutQuad = (t: float) => 1. -. (1. -. t) *. (1. -. t)

type pos = {
  x: int,
  y: int,
  z: int,
}

@decco
type stateActor = User | Auto

type autoProgress<'a> = Send('a) | SendOrAccept('a) | Seek | DoNothing | Accept

type autoProgressBase = Seek | DoNothing | Accept

type droppedUpon<'game, 'dragPile> = ('game, 'dragPile) => option<'game>

type movableSpace<'game, 'space, 'dragPile> = {
  locationAdjustment: pos,
  baseSpace: 'space,
  dragPile: unit => option<'dragPile>,
  autoProgress: unit => autoProgress<'dragPile>,
  droppedUpon: droppedUpon<'game, 'dragPile>,
  onMove: (~hide: unit => unit, ~show: unit => unit) => unit,
  onClick: 'game => option<'game>,
  // applyMoveToOthers: ('space => unit) => unit,
}

type staticSpace<'game, 'dragPile> = {
  droppedUpon: droppedUpon<'game, 'dragPile>,
  autoProgress: autoProgressBase,
}

type spaceFunction<'game, 'space, 'dragPile> =
  Movable(movableSpace<'game, 'space, 'dragPile>) | Static(staticSpace<'game, 'dragPile>)

type getRule<'game, 'space, 'dragPile> = (
  'game,
  'space,
) => option<spaceFunction<'game, 'space, 'dragPile>>

module type GameRules = {
  @decco
  type game
  type space
  type dragPile
  @decco
  type deck

  let getSpace: Element.t => option<space>
  let spaceToString: space => string
  let initiateGame: unit => (deck, game)
  let getRule: getRule<game, space, dragPile>
  let removeDragFromGame: (game, dragPile) => game
  let winCheck: game => bool
  let applyLiftToDragPile: (dragPile, (space, int) => unit) => unit
  let applyMoveToDragPile: (dragPile, (space, int, int) => unit) => unit

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
    type props<'setRef, 'onMouseDown, 'onClick, 'deck> = {
      setRef: 'setRef,
      onMouseDown: 'onMouseDown,
      onClick: 'onClick,
      deck: 'deck,
    }
    let make: props<
      space => ReactDOM.Ref.callbackDomRef,
      JsxEventU.Mouse.t => unit,
      JsxEventU.Mouse.t => unit,
      deck,
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

  @decco
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

  @decco
  type undoStats = {
    currentUndoDepth: int,
    undos: array<int>,
  }

  @decco
  type state = {deck: GameRules.deck, history: array<historySnapshot>, undoStats: undoStats}

  let useGame = (subscribe, getInitial) => {
    let (externalState, setExternalState) = React.useState(() => getInitial())
    React.useEffect0(() => {
      let unsubscribe = subscribe(setExternalState)
      Some(() => unsubscribe())
    })
    let game = externalState

    game
  }

  module BoardWrapper = {
    @react.component
    let make = (
      ~subscribe,
      ~getGame,
      ~setRef,
      ~onMouseDown,
      ~setGame,
      ~moveToState,
      ~autoProgress,
      ~undo,
      ~createNewGame,
      ~restartGame,
    ) => {
      let game = useGame(subscribe, getGame)

      let undo = () => {
        undo()
        moveToState()
      }
      let restartGame = () => {
        restartGame()
        moveToState()
      }
      let isWin = GameRules.winCheck(game)
      <React.Fragment>
        <Common.UtilBoard undo isWin createNewGame restartGame />
        <GameRules.Board setRef onMouseDown setGame moveToState autoProgress game undo isWin />
      </React.Fragment>
    }
  }

  type dragData = {
    dragElement: Dom.element,
    offset: (int, int),
    dragSpace: GameRules.space,
    dragPile: GameRules.dragPile,
  }

  module Main = {
    @react.component
    let make = (
      ~getState: unit => state,
      ~setState: (state => state) => unit,
      ~createNewGame: unit => unit,
    ) => {
      let listeners = ref(Set.make())

      let subscribe = listener => {
        listeners.contents->Set.add(listener)
        let unsubscribe = () => listeners.contents->Set.delete(listener)->ignore
        unsubscribe
      }

      let getDeck = () => {
        getState().deck
      }

      let getGame = () => {
        let snapShot = getState().history->Array.getUnsafe(getState().history->Array.length - 1)

        snapShot.game
      }

      let setGame = f => {
        setState(state => {
          let newGame = f(getGame())
          listeners.contents->Set.forEach(listener => listener(_ => newGame))

          {
            ...state,
            undoStats: state.undoStats.currentUndoDepth > 0
              ? {
                  currentUndoDepth: 0,
                  undos: Array.concat(state.undoStats.undos, [state.undoStats.currentUndoDepth]),
                }
              : state.undoStats,
            history: Array.concat(state.history, [{game: newGame, actor: Auto}]),
          }
        })
      }

      let snapshot = () => {
        setState(state => {
          ...state,
          history: state.history->Common.ArrayAux.update(state.history->Array.length - 1, v => {
            ...v,
            actor: User,
          }),
        })
      }

      let undo = () => {
        if getState().history->Array.filter(v => v.actor == User)->Array.length > 1 {
          setState(state => {
            let newHistory = state.history->Common.ArrayAux.sliceBefore(v => v.actor == User)
            {
              ...state,
              history: newHistory,
              undoStats: {
                ...state.undoStats,
                currentUndoDepth: state.undoStats.currentUndoDepth + 1,
              },
            }
          })
        }
      }

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

      let liftUpDragPile = dragPile => {
        GameRules.applyLiftToDragPile(dragPile, (space, zIndex) => {
          space
          ->getElement
          ->Option.mapOr((), element => {
            element->setStyleZIndex((1000 + zIndex)->Int.toString)
          })
        })
      }

      let liftUp = (element, zIndex) => {
        element->setStyleZIndex(zIndex->Int.toString)
      }

      let setDown = (element, zIndex) => {
        zIndex->Option.mapOr((), zIndex => {
          element->setStyleZIndex(zIndex->Int.toString)
        })
      }

      let move = (element, left, top) => {
        element->setStyleLeft(left->Int.toString ++ "px")
        element->setStyleTop(top->Int.toString ++ "px")
      }

      let moveWithTime = (element, refPos, targetLeft, targetTop, targetZIndex, duration) => {
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

          move(element, leftMove->Int.fromFloat, topMove->Int.fromFloat)

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

      let hide = element => {
        element
        ->Element.querySelector(".card-back")
        ->Option.mapOr((), cardBackElement => {
          cardBackElement->classListRemove("hidden")
        })
      }

      let show = element => {
        element
        ->Element.querySelector(".card-back")
        ->Option.mapOr((), cardBackElement => {
          cardBackElement->classListAdd("hidden")
        })
      }

      let moveToState = () => {
        refs.current->Array.forEach(element => {
          element
          ->GameRules.getSpace
          ->Option.flatMap(space => GameRules.getRule(getGame(), space))
          ->Option.mapOr((), rule => {
            switch rule {
            | Static(_) => ()
            | Movable({locationAdjustment, baseSpace, onMove}) =>
              baseSpace
              ->getElement
              ->Option.mapOr(
                (),
                baseElement => {
                  let basePos = baseElement->elementPosition
                  onMove(~hide=() => hide(element), ~show=() => show(element))
                  moveWithTime(
                    element,
                    basePos,
                    locationAdjustment.x->Int.toFloat,
                    locationAdjustment.y->Int.toFloat,
                    Some(locationAdjustment.z),
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

      let progressDragPiles = (dragPiles, droppedUpons) => {
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
                  | SendOrAccept(dragPile)
                  | Send(dragPile) =>
                    Some(dragPile)
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
                | Static({autoProgress, droppedUpon}) =>
                  switch autoProgress {
                  | Seek => Some(droppedUpon)
                  | _ => None
                  }

                | Movable({autoProgress, droppedUpon}) =>
                  switch autoProgress() {
                  | Seek => Some(droppedUpon)
                  | _ => None
                  }
                }
              })
            })

            progressDragPiles(dragPiles, droppedUpons)
          },
        )
      }

      let onClick = event => {
        event
        ->JsxEvent.Mouse.currentTarget
        ->Obj.magic
        ->GameRules.getSpace
        ->Option.mapOr((), dragSpace => {
          GameRules.getRule(getGame(), dragSpace)->Option.mapOr((), rule => {
            switch rule {
            | Static(_) => ()
            | Movable({onClick}) =>
              onClick(getGame())->Option.mapOr(
                (),
                newGame => {
                  if (
                    getGame()->GameRules.game_encode->Js.Json.stringify !=
                      newGame->GameRules.game_encode->Js.Json.stringify
                  ) {
                    setGame(_ => newGame)
                    snapshot()
                    moveToState()
                    autoProgress()
                  }
                },
              )
            }
          })
        })
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
                  liftUpDragPile(dragPile)
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

          GameRules.applyMoveToDragPile(dragData.dragPile, (space, x, y) => {
            space
            ->getElement
            ->Option.mapOr(
              (),
              element => {
                move(element, leftMove + x, topMove + y)
              },
            )
          })
        })
      }

      let onMouseUpNone = dragPile => {
        let droppedUpons = refs.current->Array.filterMap(el => {
          el
          ->GameRules.getSpace
          ->Option.flatMap(elSpace => GameRules.getRule(getGame(), elSpace))
          ->Option.mapOr(None, rule => {
            switch rule {
            | Static({autoProgress, droppedUpon}) =>
              switch autoProgress {
              | Seek => Some(droppedUpon)
              | Accept => Some(droppedUpon)
              | DoNothing => None
              }

            | Movable({autoProgress, droppedUpon}) =>
              switch autoProgress() {
              | SendOrAccept(_)
              | Accept
              | Seek =>
                Some(droppedUpon)
              | _ => None
              }
            }
          })
        })

        if progressDragPiles([dragPile], droppedUpons->Array.toReversed) {
          snapshot()
        }
        moveToState()
        autoProgress()
      }

      let onMouseUp = _ => {
        switch dragData.current {
        | Some({dragElement, dragPile}) => {
            let greatestOverlap = ref(0.)
            let updatedGame = ref(None)
            let oldGame = getGame()

            refs.current->Array.forEach(el => {
              el
              ->GameRules.getSpace
              ->Option.flatMap(elSpace => GameRules.getRule(getGame(), elSpace))
              ->Option.flatMap(rule => {
                let droppedUpon = switch rule {
                | Static({droppedUpon}) => droppedUpon
                | Movable({droppedUpon}) => droppedUpon
                }

                droppedUpon(oldGame->GameRules.removeDragFromGame(dragPile), dragPile)
              })
              ->Option.mapOr((), newGame => {
                let overlap = getOverlap(el, dragElement)
                if overlap > greatestOverlap.contents {
                  greatestOverlap := overlap
                  if (
                    oldGame->GameRules.game_encode->Js.Json.stringify !=
                      newGame->GameRules.game_encode->Js.Json.stringify
                  ) {
                    updatedGame := Some(newGame)
                  }
                }
              })
            })

            switch updatedGame.contents {
            | None => onMouseUpNone(dragPile)
            | Some(updatedGame) => {
                setGame(_ => updatedGame)
                snapshot()
                moveToState()
                autoProgress()
              }
            }
          }
        | None => ()
        }

        Console.log(getGame())

        dragData.current = None
      }

      let restartGame = () => {
        setState(state => {
          {
            ...state,
            history: state.history->Array.slice(~start=0, ~end=1),
            undoStats: {
              currentUndoDepth: 0,
              undos: [],
            },
          }
        })
        moveToState()
        autoProgress()
      }

      React.useEffect(() => {
        window->Window.addMouseMoveEventListener(onMouseMove)
        window->Window.addMouseUpEventListener(onMouseUp)
        window->Window.addKeyDownEventListener(event => {
          if event->KeyboardEvent.key == "z" {
            undo()
            moveToState()
          }
        })

        moveToState()
        autoProgress()
        None
      }, [])

      <div id={"board"} className="relative m-5 mt-0">
        <BoardWrapper
          createNewGame
          subscribe
          getGame
          onMouseDown
          setRef
          setGame
          moveToState
          autoProgress
          restartGame
          undo
        />
        <GameRules.AllCards onMouseDown onClick setRef deck={getDeck()} />
      </div>
    }
  }

  @react.component
  let make = (
    ~getState: option<unit => state>,
    ~setState: (state => state) => unit,
    ~onCreateNewGame: state => unit,
  ) => {
    let createNewGame = () => {
      let (deck, game) = GameRules.initiateGame()
      let newGame = {
        deck,
        history: [
          {
            actor: User,
            game,
          },
        ],
        undoStats: {
          currentUndoDepth: 0,
          undos: [],
        },
      }

      onCreateNewGame(newGame)
    }

    React.useEffect0(() => {
      if getState->Option.isNone {
        createNewGame()
      }
      None
    })

    switch getState {
    | None => React.null
    | Some(getState) => <Main getState setState createNewGame />
    }
  }
}
