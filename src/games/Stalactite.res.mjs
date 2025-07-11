// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Card from "../Card.res.mjs";
import * as Bases from "../Bases.res.mjs";
import * as Rules from "../Rules.res.mjs";
import * as Boards from "../Boards.res.mjs";
import * as Common from "../Common.res.mjs";
import * as GameBase from "../GameBase.res.mjs";
import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";

function initiateGame() {
  var shuffledDeck = Core__Array.toShuffled(Card.getDeck(0, false));
  var deckToDeal = {
    contents: shuffledDeck
  };
  return [
          shuffledDeck,
          {
            tableau: [
              Common.ArrayAux.popN(deckToDeal, 6),
              Common.ArrayAux.popN(deckToDeal, 6),
              Common.ArrayAux.popN(deckToDeal, 6),
              Common.ArrayAux.popN(deckToDeal, 6),
              Common.ArrayAux.popN(deckToDeal, 6),
              Common.ArrayAux.popN(deckToDeal, 6),
              Common.ArrayAux.popN(deckToDeal, 6),
              Common.ArrayAux.popN(deckToDeal, 6)
            ],
            foundations: [
              Common.ArrayAux.popN(deckToDeal, 1),
              Common.ArrayAux.popN(deckToDeal, 1),
              Common.ArrayAux.popN(deckToDeal, 1),
              Common.ArrayAux.popN(deckToDeal, 1)
            ],
            foundations2: [],
            stock: [],
            waste: [],
            free: [
              undefined,
              undefined
            ]
          }
        ];
}

var forEachSpace = Bases.Stalactite.makeForEachSpace(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, Rules.FreeCell.freeBaseRules, Rules.FreeCell.freeRules);

var Game = GameBase.Create({
      game_encode: Bases.Stalactite.game_encode,
      game_decode: Bases.Stalactite.game_decode,
      deck_encode: Bases.Stalactite.deck_encode,
      deck_decode: Bases.Stalactite.deck_decode,
      getSpace: Bases.Stalactite.getSpace,
      spaceToString: Bases.Stalactite.spaceToString,
      initiateGame: initiateGame,
      forEachSpace: forEachSpace,
      removeDragFromGame: Bases.Stalactite.removeDragFromGame,
      winCheck: Bases.Stalactite.winCheck,
      applyLiftToDragPile: Bases.Stalactite.applyLiftToDragPile,
      applyMoveToDragPile: Bases.Stalactite.applyMoveToDragPile,
      Board: Boards.FRT,
      AllCards: Bases.Stalactite.AllCards
    });

export {
  Game ,
}
/* Game Not a pure module */
