// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Card from "../Card.res.mjs";
import * as Bases from "../Bases.res.mjs";
import * as Rules from "../Rules.res.mjs";
import * as Boards from "../Boards.res.mjs";
import * as Common from "../Common.res.mjs";
import * as GameBase from "../GameBase.res.mjs";
import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";
import * as GameCommons from "../GameCommons.res.mjs";

function winCheck(game) {
  return game.tableau.every(function (pile) {
              if (pile.length === 13) {
                return GameCommons.decCyclicAnySuitValidation(pile);
              } else {
                return false;
              }
            });
}

function initiateGame() {
  var shuffledDeck = Core__Array.toShuffled([].concat(Card.getDeck(0, true), Card.getDeck(1, true)));
  var deckToDeal = {
    contents: shuffledDeck
  };
  return [
          shuffledDeck,
          {
            tableau: GameCommons.flipLastUp([
                  Common.ArrayAux.popN(deckToDeal, 1),
                  Common.ArrayAux.popN(deckToDeal, 1),
                  Common.ArrayAux.popN(deckToDeal, 1),
                  Common.ArrayAux.popN(deckToDeal, 1),
                  Common.ArrayAux.popN(deckToDeal, 1),
                  Common.ArrayAux.popN(deckToDeal, 1),
                  Common.ArrayAux.popN(deckToDeal, 1),
                  Common.ArrayAux.popN(deckToDeal, 1)
                ]),
            foundations: [],
            foundations2: [],
            stock: [
              Common.ArrayAux.popN(deckToDeal, 8),
              Common.ArrayAux.popN(deckToDeal, 8),
              Common.ArrayAux.popN(deckToDeal, 8),
              Common.ArrayAux.popN(deckToDeal, 8),
              Common.ArrayAux.popN(deckToDeal, 8),
              Common.ArrayAux.popN(deckToDeal, 8),
              Common.ArrayAux.popN(deckToDeal, 8),
              Common.ArrayAux.popN(deckToDeal, 8),
              Common.ArrayAux.popN(deckToDeal, 8),
              Common.ArrayAux.popN(deckToDeal, 8),
              Common.ArrayAux.popN(deckToDeal, 8),
              Common.ArrayAux.popN(deckToDeal, 8)
            ],
            waste: [],
            free: []
          }
        ];
}

var forEachSpace = Bases.GermanPatience.makeForEachSpace(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, Rules.DealAll.stockRules, undefined, undefined);

var Game = GameBase.Create({
      game_encode: Bases.GermanPatience.game_encode,
      game_decode: Bases.GermanPatience.game_decode,
      deck_encode: Bases.GermanPatience.deck_encode,
      deck_decode: Bases.GermanPatience.deck_decode,
      getSpace: Bases.GermanPatience.getSpace,
      spaceToString: Bases.GermanPatience.spaceToString,
      initiateGame: initiateGame,
      forEachSpace: forEachSpace,
      removeDragFromGame: Bases.GermanPatience.removeDragFromGame,
      winCheck: winCheck,
      applyLiftToDragPile: Bases.GermanPatience.applyLiftToDragPile,
      applyMoveToDragPile: Bases.GermanPatience.applyMoveToDragPile,
      Board: Boards.ST,
      AllCards: Bases.GermanPatience.AllCards
    });

export {
  Game ,
}
/* Game Not a pure module */
