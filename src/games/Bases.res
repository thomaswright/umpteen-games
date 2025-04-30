module BakersGame = Packer.Make({
  let spec: Packer.spec = {
    drop: OneSuit,
    drag: OneSuit,
    size: FreeSize,
    depot: AnyDepot,
    foundation: ByOne,
  }
})

module FreeCell = Packer.Make({
  let spec: Packer.spec = {
    drop: AltSuit,
    drag: AltSuit,
    size: FreeSize,
    depot: AnyDepot,
    foundation: ByOne,
  }
})

module SeahavenTowers = Packer.Make({
  let spec: Packer.spec = {
    drop: OneSuit,
    drag: OneSuit,
    size: FreeSize,
    depot: SpecificDepot(RK),
    foundation: ByOne,
  }
})

module Penguin = Packer.Make({
  let spec: Packer.spec = {
    drop: CyclicOneSuit,
    drag: CyclicOneSuit,
    size: AnySize,
    depot: AnyDepot, // will override
    foundation: ByOneCyclicOneSuit,
  }
})

module Stalactite = Packer.Make({
  let spec: Packer.spec = {
    drop: NoDrop,
    drag: AnySuit,
    size: JustOne,
    depot: AnyDepot, // will override
    foundation: ByOneCyclicAnySuit,
  }
})
