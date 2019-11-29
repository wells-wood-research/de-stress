import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

// {{{ Elm App

const specificationStore = new idbKeyval.Store(
  "destress-front-end",
  "specification-store"
);

var app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: { randomSeed: Math.floor(Math.random() * 0x0fffffff) }
});

// }}}
// {{{ Ports

// Store specifications
app.ports.storeSpecification.subscribe(specificationAndKey => {
  var { storeKey, specification } = specificationAndKey;
  if (idbAvailable) {
    idbKeyval.set(storeKey, specification, specificationStore);
  } else {
    console.log(
      "Storage is not available. IndexedDB must be enabled to store state."
    );
  }
});

//Get specification
app.ports.getSpecification.subscribe(storeKey => {
  idbKeyval.get(storeKey, specificationStore).then(specification =>
    app.ports.setFocussedSpecification.send({
      uuidString: storeKey,
      specification: specification
    })
  );
});

// }}}
// {{{ Utilities

function idbAvailable() {
  return Boolean(window.indexedDB);
}

// }}}
// {{{ Service Worker

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

// }}}
