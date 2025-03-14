%%raw("import './index.css'")

switch ReactDOM.querySelector("#root") {
| Some(domElement) =>
  ReactDOM.Client.createRoot(domElement)->ReactDOM.Client.Root.render(
    // <React.StrictMode>
    <UpAndDown.Game />,
    // </React.StrictMode>,
  )
| None => ()
}
