open Packer

// Stock, Waste, Foundation, Piles
module Klondike = {
  @react.component
  let make = (~setRef, ~initialGame: game) => {
    <React.Fragment>
      <div className="flex flex-row gap-3">
        <div
          key={Stock->spaceToString}
          id={Stock->spaceToString}
          ref={ReactDOM.Ref.callbackDomRef(setRef(Stock))}
          className=" bg-black opacity-20 rounded w-14 h-20"
        />
        <div
          key={Waste->spaceToString}
          id={Waste->spaceToString}
          ref={ReactDOM.Ref.callbackDomRef(setRef(Waste))}
          className=" w-14 h-20"
        />
      </div>
      <div className="flex flex-row gap-3 mt-5">
        {Array.make(~length=initialGame.foundations->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Foundation(i)->spaceToString}
            id={Foundation(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
            className=" bg-white opacity-10 rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
      <div className="flex flex-row gap-3 mt-5">
        {Array.make(~length=initialGame.piles->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Pile(i)->spaceToString}
            id={Pile(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
            className=" bg-black opacity-20  rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Foundation, Frees, Piles
module FreeCell = {
  @react.component
  let make = (~setRef, ~initialGame: game) => {
    <React.Fragment>
      <div className="flex flex-row gap-3">
        <div className="flex flex-row gap-3">
          {Array.make(~length=initialGame.free->Array.length, [])
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Free(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i)))}
              className=" bg-black opacity-20   rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
        {Array.make(
          ~length=initialGame.piles->Array.length -
          initialGame.free->Array.length -
          initialGame.foundations->Array.length,
          <div className="w-14 h-20" />,
        )->React.array}
        <div className="flex flex-row gap-3">
          {Array.make(~length=initialGame.foundations->Array.length, [])
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Foundation(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
              className=" bg-white opacity-10  rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
      </div>
      <div />
      <div className="flex flex-row gap-3 mt-5">
        {Array.make(~length=initialGame.piles->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Pile(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
            className=" bg-black opacity-20   rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Foundation, Frees, Piles
module DoubleFreeCell = {
  @react.component
  let make = (~setRef, ~initialGame: game) => {
    <React.Fragment>
      <div className="flex flex-row gap-3">
        <div className="grid grid-cols-4 gap-3">
          {Array.make(~length=initialGame.free->Array.length, [])
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Free(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i)))}
              className="   bg-black opacity-20  rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
        {Array.make(~length=2, <div className="w-14 h-20" />)->React.array}
        <div className="grid grid-cols-4 gap-3 ">
          {Array.make(~length=initialGame.foundations->Array.length, [])
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Foundation(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
              className="   bg-white opacity-10  rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
      </div>
      <div />
      <div className="flex flex-row gap-3 mt-5">
        {Array.make(~length=initialGame.piles->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Pile(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
            className=" bg-black opacity-20  rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Foundation, Frees, Piles
module EightOff = {
  @react.component
  let make = (~setRef, ~initialGame: game) => {
    <React.Fragment>
      <div className="flex flex-row">
        <div className="flex flex-col gap-3 mr-6">
          {Array.make(~length=initialGame.foundations->Array.length, [])
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Foundation(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
              className=" bg-white opacity-10  rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
        <div>
          <div className="flex flex-row">
            <div className="flex flex-row gap-3">
              {Array.make(~length=initialGame.free->Array.length, [])
              ->Array.mapWithIndex((_, i) => {
                <div
                  key={Free(i)->spaceToString}
                  ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i)))}
                  className=" bg-black opacity-20   rounded w-14 h-20"
                />
              })
              ->React.array}
            </div>
          </div>
          <div />
          <div className="flex flex-row gap-3 mt-3">
            {Array.make(~length=initialGame.piles->Array.length, [])
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Pile(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
                className=" bg-black opacity-20   rounded w-14 h-20"
              />
            })
            ->React.array}
          </div>
        </div>
      </div>
    </React.Fragment>
  }
}

// Stock, Foundation, Piles
module Spider = {
  @react.component
  let make = (~setRef, ~initialGame: game) => {
    <React.Fragment>
      <div className="flex flex-row">
        <div
          key={Stock->spaceToString}
          ref={ReactDOM.Ref.callbackDomRef(setRef(Stock))}
          className=" bg-white opacity-10  rounded w-14 h-20 mr-20"
        />
        <div className="flex flex-row gap-3 ml-10">
          {Array.make(~length=initialGame.foundations->Array.length, [])
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Foundation(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
              className=" bg-white opacity-10  rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
      </div>
      <div />
      <div className="flex flex-row gap-3 mt-5">
        {Array.make(~length=initialGame.piles->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Pile(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
            className=" bg-black opacity-20   rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Stock, Foundation, Piles
module AgnesBernauer = {
  @react.component
  let make = (~setRef, ~initialGame: game) => {
    <React.Fragment>
      <div className="flex flex-row">
        <div
          key={Stock->spaceToString}
          ref={ReactDOM.Ref.callbackDomRef(setRef(Stock))}
          className=" bg-white opacity-10  rounded w-14 h-20 mr-20"
        />
        <div className="flex flex-row gap-3 ml-10">
          {Array.make(~length=initialGame.foundations->Array.length, [])
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Foundation(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
              className=" bg-white opacity-10  rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
      </div>
      <div className="flex flex-row gap-3 mt-5">
        {Array.make(~length=initialGame.free->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Free(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i)))}
            className=" bg-black opacity-20   rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
      <div className="flex flex-row gap-3 mt-5">
        {Array.make(~length=initialGame.piles->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Pile(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
            className=" bg-black opacity-20   rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Foundation, Piles
module SimpleSimon = {
  @react.component
  let make = (~setRef, ~initialGame: game) => {
    <React.Fragment>
      <div className="flex flex-row gap-3 mt-5">
        {Array.make(~length=initialGame.foundations->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Foundation(i)->spaceToString}
            id={Foundation(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
            className=" bg-white opacity-10 rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
      <div className="flex flex-row gap-3 mt-5">
        {Array.make(~length=initialGame.piles->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Pile(i)->spaceToString}
            id={Pile(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
            className=" bg-black opacity-20  rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Stock, Piles
module GermanPatience = {
  @react.component
  let make = (~setRef, ~initialGame: game) => {
    <React.Fragment>
      <div className="flex flex-row">
        <div
          key={Stock->spaceToString}
          ref={ReactDOM.Ref.callbackDomRef(setRef(Stock))}
          className=" bg-white opacity-10  rounded w-14 h-20 mr-20"
        />
      </div>
      <div />
      <div className="flex flex-row gap-3 mt-5">
        {Array.make(~length=initialGame.piles->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Pile(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
            className=" bg-black opacity-20   rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}
