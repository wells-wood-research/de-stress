// On load, listen to Elm!
window.addEventListener("load", _ => {
  window.ports = {
    init: app =>
      app.ports.outgoing.subscribe(({ action, data }) => {
        actions[action]
          ? actions[action](app, data)
          : console.warn(`I didn't recognize action "${action}".`);
      })
  };
});

// {{{ Run State
// Get stored global state
const storageKey = "globalState";

// Store Run State
const storeRunState = (_, storedRunState) => {
  if (storageAvailable("localStorage")) {
    localStorage.setItem(storageKey, JSON.stringify(storedRunState));
  } else {
    console.log(
      "Storage is not available. Local storage must be enabled to store state."
    );
  }
};
// }}}
// {{{ Designs
//
const designStore = new idbKeyval.Store("designs", "design-store");

// Store design
const storeDesign = (_, designAndKey) => {
  var { storeKey, design } = designAndKey;
  if (idbAvailable) {
    idbKeyval.set(storeKey, design, designStore);
  } else {
    console.log(
      "Storage is not available. IndexedDB must be enabled to store state."
    );
  }
};

// Get design
const getDesign = (app, storeKey) => {
  idbKeyval.get(storeKey, designStore).then(design => {
    app.ports.setFocussedDesign.send({
      uuidString: storeKey,
      design: design
    });
  });
};

// Delete design
const deleteDesign = (_, storeKey) => {
  idbKeyval.del(storeKey, designStore);
};

// }}}
// {{{ Specifications
const specificationStore = new idbKeyval.Store(
  "specifications",
  "specification-store"
);

// Store specification
const storeSpecification = (_, specificationAndKey) => {
  var { storeKey, specification } = specificationAndKey;
  if (idbAvailable) {
    idbKeyval.set(storeKey, specification, specificationStore);
  } else {
    console.log(
      "Storage is not available. IndexedDB must be enabled to store state."
    );
  }
};

// Get specification
const getSpecification = (app, storeKey) => {
  idbKeyval.get(storeKey, specificationStore).then(specification => {
    app.ports.setFocussedSpecification.send({
      uuidString: storeKey,
      specification: specification
    });
  });
};

// Delete specification
const deleteSpecification = (_, storeKey) => {
  idbKeyval.del(storeKey, specificationStore);
};

// }}}

// maps actions to functions!
const actions = {
  // Run State
  STORE_STATE: storeRunState,
  // Designs
  STORE_DESIGN: storeDesign,
  GET_DESIGN: getDesign,
  DELETE_DESIGN: deleteDesign,
  // Specifications
  STORE_SPECIFICATION: storeSpecification,
  GET_SPECIFICATION: getSpecification,
  DELETE_SPECIFICATION: deleteSpecification
};

// {{{ Utilities

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
