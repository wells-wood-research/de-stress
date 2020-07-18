import {
  Store,
  clear,
  del,
  set,
} from "https://cdn.jsdelivr.net/npm/idb-keyval@3/dist/idb-keyval.mjs";

// {{{ storage

const localStorageKey = "globalState";
const designStore = new Store("designs", "design-store");

// {{{ utilities

function idbAvailable() {
  return Boolean(window.indexedDB);
}

// Taken from MDN
function storageAvailable(type) {
  var storage;
  try {
    storage = window[type];
    var x = "__storage_test__";
    storage.setItem(x, x);
    storage.removeItem(x);
    return true;
  } catch (e) {
    return (
      e instanceof DOMException &&
      // everything except Firefox
      (e.code === 22 ||
        // Firefox
        e.code === 1014 ||
        // test name field too, because code might not be present
        // everything except Firefox
        e.name === "QuotaExceededError" ||
        // Firefox
        e.name === "NS_ERROR_DOM_QUOTA_REACHED") &&
      // acknowledge QuotaExceededError only if there's something already stored
      storage &&
      storage.length !== 0
    );
  }
}

// }}}

// }}}
// {{{ flags
// Initial data passed to Elm (should match `Flags` defined in `Shared.elm`)
// https://guide.elm-lang.org/interop/flags.html

const storedState = localStorage.getItem(localStorageKey);
const mInitialState = storedState ? JSON.parse(storedState) : null;
const flags = {
  initialSeed: Math.floor(Math.random() * 0x0fffffff),
  mInitialState,
};

// }}}
// {{{ app

// Start our Elm application
var app = Elm.Main.init({ flags: flags });

// }}}
// {{{ ports
// https://guide.elm-lang.org/interop/ports.html

// {{{ RunState

app.ports.storeRunState.subscribe((runState) => {
  if (storageAvailable("localStorage")) {
    localStorage.setItem(localStorageKey, JSON.stringify(runState));
  } else {
    console.log(
      "Storage is not available. Local storage must be enabled to save the application."
    );
  }
});
// }}}

// {{{ Designs

app.ports.storeDesign.subscribe((uuidStringAndDesign) => {
  const { uuidString, design } = uuidStringAndDesign;
  if (idbAvailable) {
    set(uuidString, design, designStore);
  } else {
    console.log(
      "Storage is not available. IndexedDB must be enabled to store state."
    );
  }
});

app.ports.deleteDesign.subscribe(({ uuidString }) => {
  del(uuidString, designStore);
});

app.ports.deleteAllDesigns.subscribe(() => {
  clear(designStore);
});
// }}}

// }}}
