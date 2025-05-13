@val @module("./other.js")
external numInterval: (int => unit, int, int) => promise<unit> = "numInterval"

@val @module("./other.js")
external triggerConfetti: unit => unit = "triggerConfetti"

module ArrayAux = {
  let removeLast = a => a->Array.toReversed->Array.sliceToEnd(~start=1)->Array.toReversed
  let getLast = a => a->Array.toReversed->Array.get(0)
  let update = (a, i, f) => a->Array.mapWithIndex((el, j) => j == i ? f(el) : el)
  let updateLast = (a, f) => a->Array.mapWithIndex((el, j) => j == a->Array.length - 1 ? f(el) : el)

  let insertAfter = (arr, match, new) =>
    arr->Array.reduce([], (acc, c) => {
      if c == match {
        Array.concat(acc, Array.concat([c], new))
      } else {
        Array.concat(acc, [c])
      }
    })

  let forEach2 = (a, f) =>
    a->Array.forEachWithIndex((el1, i) => {
      el1->Array.forEachWithIndex((el2, j) => {
        f(el1, el2, i, j)
      })
    })

  let sliceBefore = (arr, pred) => {
    let revIndex = arr->Array.toReversed->Array.findIndex(pred)
    arr->Array.slice(~start=0, ~end=arr->Array.length - 1 - revIndex)
  }

  let popN = (deck, n) => {
    let result = deck.contents->Array.slice(~start=0, ~end=n)
    deck := deck.contents->Array.sliceToEnd(~start=n)
    result
  }
}

module UtilBoard = {
  @react.component
  let make = (~undo, ~isWin, ~createNewGame, ~restartGame) => {
    React.useEffect1(() => {
      if isWin {
        triggerConfetti()
      }
      None
    }, [isWin])
    <div className="flex flex-row mb-5 mt-2 gap-2">
      <button className={"bg-[#edffe1] rounded-lg px-4 "} onClick={_ => undo()}>
        {"Undo"->React.string}
      </button>
      <button className={"bg-[#edffe1] rounded-lg px-4 "} onClick={_ => restartGame()}>
        {"Restart"->React.string}
      </button>
      <div className={"w-36"} />
      <button className={"bg-[#edffe1] rounded-lg px-4 "} onClick={_ => createNewGame()}>
        {"New Game"->React.string}
      </button>
      // <button className={"bg-gray-200 rounded px-4 "} onClick={_ => triggerConfetti()}>
      //   {"Trigger Confetti"->React.string}
      // </button>
      <div className="px-4 font-black text-amber-400">
        {isWin ? "You Won!"->React.string : React.null}
      </div>
    </div>
  }
}

@module("./useLocalStorage.js")
external useLocalStorage: (string, 'a) => ('a, ('a => 'a) => unit, unit => 'a) = "default"
