@val @module("./other.js")
external numInterval: (int => unit, int, int) => promise<unit> = "numInterval"

module ArrayAux = {
  let removeLast = a => a->Array.toReversed->Array.sliceToEnd(~start=1)->Array.toReversed
  let getLast = a => a->Array.toReversed->Array.get(0)
  let update = (a, i, f) => a->Array.mapWithIndex((el, j) => j == i ? f(el) : el)
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
}
