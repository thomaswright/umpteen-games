open Packer

// Stock, Waste, Foundation, Foundation2, Reserve
module SWFFR = {
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
        {Array.make(~length=initialGame.foundations2->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Foundation2(i)->spaceToString}
            id={Foundation2(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation2(i)))}
            className=" bg-white opacity-10 rounded w-14 h-20"
          />
        })
        ->React.array}
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
    </React.Fragment>
  }
}

// Stock, Waste, Foundation, Tableau
module SWFT = {
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
        {Array.make(~length=initialGame.tableau->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Tableau(i)->spaceToString}
            id={Tableau(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Tableau(i)))}
            className=" bg-black opacity-20  rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Foundation, Reserve, Tableau
module FRT = {
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
          ~length=initialGame.tableau->Array.length -
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
        {Array.make(~length=initialGame.tableau->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Tableau(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Tableau(i)))}
            className=" bg-black opacity-20   rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Foundation, Reserve, Tableau
module FRT2 = {
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
        {Array.make(~length=initialGame.tableau->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Tableau(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Tableau(i)))}
            className=" bg-black opacity-20  rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Foundation, Reserve, Tableau
module FRT3 = {
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
            {Array.make(~length=initialGame.tableau->Array.length, [])
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Tableau(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Tableau(i)))}
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

// Stock, Foundation, Tableau
module SFT = {
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
        {Array.make(~length=initialGame.tableau->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Tableau(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Tableau(i)))}
            className=" bg-black opacity-20   rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Stock, Foundation, tableau, Free
module SFTR = {
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
        {Array.make(~length=initialGame.tableau->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Tableau(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Tableau(i)))}
            className=" bg-black opacity-20   rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Foundation, Tableau
module FT = {
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
        {Array.make(~length=initialGame.tableau->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Tableau(i)->spaceToString}
            id={Tableau(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Tableau(i)))}
            className=" bg-black opacity-20  rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Todo: align clock with tableau
// Foundation, Tableau
module FT_Clock = {
  @react.component
  let make = (~setRef, ~initialGame: game) => {
    <React.Fragment>
      <div
        style={{
          height: "450px",
          width: "360px",
        }}
        className="relative">
        {Array.make(~length=initialGame.foundations->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          let angle = (30 * (i - 2))->Int.toFloat
          let x = 180. +. 180. *. Math.cos(2. *. 3.1415 *. angle /. 360.)
          let y = 180. +. 180. *. Math.sin(2. *. 3.1415 *. angle /. 360.)

          <div
            key={Foundation(i)->spaceToString}
            id={Foundation(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
            style={{
              left: x->Float.toInt->Int.toString ++ "px",
              top: y->Float.toInt->Int.toString ++ "px",
            }}
            className=" bg-white opacity-10 rounded w-14 h-20 absolute"
          />
        })
        ->React.array}
      </div>
      <div className="flex flex-row gap-3 mt-5">
        {Array.make(~length=initialGame.tableau->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Tableau(i)->spaceToString}
            id={Tableau(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Tableau(i)))}
            className=" bg-black opacity-20  rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}

// Stock, Tableau
module ST = {
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
        {Array.make(~length=initialGame.tableau->Array.length, [])
        ->Array.mapWithIndex((_, i) => {
          <div
            key={Tableau(i)->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Tableau(i)))}
            className=" bg-black opacity-20   rounded w-14 h-20"
          />
        })
        ->React.array}
      </div>
    </React.Fragment>
  }
}
