import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

// {{{ Elm App

const designStore = new idbKeyval.Store("designs", "design-store");
const specificationStore = new idbKeyval.Store(
  "specifications",
  "specification-store"
);

// Get stored global state
const storageKey = "globalState";
const storedState = localStorage.getItem(storageKey);
const mInitialState = storedState ? JSON.parse(storedState) : null;

var app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    randomSeed: Math.floor(Math.random() * 0x0fffffff),
    mInitialState: mInitialState
  }
});

// }}}
// {{{ Ports

// Store global state
app.ports.storeRunState.subscribe(storedRunState => {
  localStorage.setItem(storageKey, JSON.stringify(storedRunState));
});

// Store design
app.ports.storeDesign.subscribe(designAndKey => {
  var { storeKey, design } = designAndKey;
  if (idbAvailable) {
    idbKeyval.set(storeKey, design, designStore);
  } else {
    console.log(
      "Storage is not available. IndexedDB must be enabled to store state."
    );
  }
});

//Get design
app.ports.getDesign.subscribe(storeKey => {
  idbKeyval.get(storeKey, designStore).then(design => {
    app.ports.setFocussedDesign.send({
      uuidString: storeKey,
      design: design
    });
  });
});

// Store design
app.ports.deleteDesign.subscribe(storeKey => {
  idbKeyval.del(storeKey, designStore);
});

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
  idbKeyval.get(storeKey, specificationStore).then(specification => {
    app.ports.setFocussedSpecification.send({
      uuidString: storeKey,
      specification: specification
    });
  });
});

// Store specifications
app.ports.deleteSpecification.subscribe(storeKey => {
  idbKeyval.del(storeKey, specificationStore);
});

// View structure with PV
// Shows a protein structure with PV.
app.ports.viewStructure.subscribe(pdbString => {
  window.requestAnimationFrame(() => {
    const options = {
      width: "auto",
      height: "auto",
      antialias: true,
      quality: "medium"
    };
    // insert the viewer under the Dom element with id 'gl'.
    var viewer = pv.Viewer(document.getElementById("viewer"), options);
    viewer.fitParent();
    var structure = pv.io.pdb(pdbString);
    viewer.fitTo(structure);
    viewer.trace("trace", structure, { color: pv.color.byChain() });
  });
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
