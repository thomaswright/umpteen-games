open Packer

module FreeCell = {
  @react.component
  let make = (~setRef, ~initialGame: game) => {
    <React.Fragment>
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

module DoubleFreeCell = {
  @react.component
  let make = (~setRef, ~initialGame: game) => {
    <React.Fragment>
      <div className="flex flex-row">
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
        <div className="grid grid-cols-4 gap-3 ml-20">
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

module EightOff = {
  @react.component
  let make = (~setRef, ~initialGame: game) => {
    <React.Fragment>
      <div className="flex flex-row">
        <div className="flex flex-col gap-3 mr-5">
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
        </div>
      </div>
    </React.Fragment>
  }
}
