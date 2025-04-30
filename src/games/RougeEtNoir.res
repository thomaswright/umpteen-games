// module RougeEtNoirBase = Packer.Make({
//   let spec: Packer.spec = {
//     drop: AltSuit,
//     drag: AltSuit,
//     size: AnySize,
//     depot: SpecificDepot(RK),
//     foundation: ByOne,
//   }
// })

// module RougeEtNoirRules = {
//   include RougeEtNoirBase

//   let initiateGame = (): (array<Card.sides>, Packer.game) => {
//     let shuffledDeck =
//       Array.concatMany([], [Card.getDeck(0, true), Card.getDeck(1, true)])->Array.toShuffled

//     let deckToDeal = ref(shuffledDeck)

//     (
//       shuffledDeck,
//       {
//         piles: [
//           deckToDeal->ArrayAux.popN(8),
//           deckToDeal->ArrayAux.popN(7),
//           deckToDeal->ArrayAux.popN(6),
//           deckToDeal->ArrayAux.popN(5),
//           deckToDeal->ArrayAux.popN(4),
//           deckToDeal->ArrayAux.popN(3),
//           deckToDeal->ArrayAux.popN(2),
//           deckToDeal->ArrayAux.popN(1),
//           deckToDeal->ArrayAux.popN(1),
//           [],
//         ]->flipLastUp,
//         foundations: [[], [], [], [], [], [], [], []],
//         stock: [
//           deckToDeal->ArrayAux.popN(10),
//           deckToDeal->ArrayAux.popN(10),
//           deckToDeal->ArrayAux.popN(10),
//           deckToDeal->ArrayAux.popN(10),
//           deckToDeal->ArrayAux.popN(10),
//           deckToDeal->ArrayAux.popN(10),
//           deckToDeal->ArrayAux.popN(8),
//         ],
//         waste: [],
//         free: [],
//       },
//     )
//   }
//   module Board = SimpleSimonRules.Board
// }

// module RougeEtNoir = GameBase.Create(RougeEtNoirRules)

