open Packer

module Klondike = Make({
  let spec = {
    drop: AltSuit,
    drag: AltSuit,
    size: AnySize,
    depot: SpecificDepot(RK),
    foundation: ByOne,
  }
})

module BakersGame = Make({
  let spec = {
    drop: OneSuit,
    drag: OneSuit,
    size: FreeSize,
    depot: AnyDepot,
    foundation: ByOne,
  }
})

module FreeCell = Make({
  let spec = {
    drop: AltSuit,
    drag: AltSuit,
    size: FreeSize,
    depot: AnyDepot,
    foundation: ByOne,
  }
})

module SeahavenTowers = Make({
  let spec = {
    drop: OneSuit,
    drag: OneSuit,
    size: FreeSize,
    depot: SpecificDepot(RK),
    foundation: ByOne,
  }
})

module Penguin = Make({
  let spec = {
    drop: CyclicOneSuit,
    drag: CyclicOneSuit,
    size: AnySize,
    depot: AnyDepot, // will override
    foundation: ByOneCyclicOneSuit,
  }
})

module AgnesSorel = Make({
  let spec = {
    drop: CyclicSameColor,
    drag: CyclicOneSuit,
    size: AnySize,
    depot: AnyDepot,
    foundation: ByOneCyclicOneSuit,
  }
})

module Stalactite = Make({
  let spec = {
    drop: NoDrop,
    drag: AnySuit,
    size: JustOne,
    depot: AnyDepot, // will override
    foundation: ByOneCyclicAnySuit,
  }
})

module Spider = Make({
  let spec = {
    drop: AnySuit,
    drag: OneSuit,
    size: AnySize,
    depot: AnyDepot,
    foundation: ByAll,
  }
})

module Scorpion = Make({
  let spec = {
    drop: OneSuit,
    drag: AnySuit,
    size: AnySize,
    depot: SpecificDepot(RK),
    foundation: ByAll,
  }
})

module GermanPatience = Make({
  let spec = {
    drop: CyclicAnySuit,
    drag: CyclicAnySuit,
    size: AnySize,
    depot: AnyDepot,
    foundation: NoFoundation,
  }
})

module EastHaven = Make({
  let spec = {
    drop: AltSuit,
    drag: AltSuit,
    size: AnySize,
    depot: AnyDepot,
    foundation: ByOne,
  }
})

module GayGordons = Make({
  let spec = {
    drop: NoDrop,
    drag: NoDrop,
    size: JustOne,
    depot: AnyDepot,
    foundation: ByOne,
  }
})
