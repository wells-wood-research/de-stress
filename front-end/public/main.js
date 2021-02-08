import {
  Store,
  clear,
  del,
  get,
  set,
} from "https://cdn.jsdelivr.net/npm/idb-keyval@3/dist/idb-keyval.mjs";

// {{{ storage

const localStorageKey = "globalState";

const designStore = new Store("designs", "design-store");
const referenceSetStore = new Store("referenceSets", "reference-set-store");
const specificationStore = new Store("specifications", "specification-store");

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
// {{{ websockets
function connect_to_server(app) {
  sessionCommsSocket = new WebSocket("ws://localhost:8181/app-comms");

  sessionCommsSocket.onopen = function () {
    // subscribe to some channels
    app.ports.setWebSocketConnectionStatus.send(true);
    console.log("Established connection to server.");
  };

  sessionCommsSocket.onmessage = function (event) {
    app.ports.incoming.send(JSON.parse(event.data.replace(/\bNaN\b/g, "null")));
  };

  sessionCommsSocket.onclose = function (event) {
    console.log(
      "Socket is closed. Reconnect will be attempted in 1 second.",
      event.reason
    );
    app.ports.setWebSocketConnectionStatus.send(false);
    setTimeout(function () {
      connect_to_server(app);
    }, 1000);
  };

  sessionCommsSocket.onerror = function (err) {
    console.error("Socket encountered error: ", err.message, "Closing socket");
    sessionCommsSocket.close();
  };
  return sessionCommsSocket;
}

// }}}
// {{{ app
// Start our Elm application
var app = Elm.Main.init({ flags: flags });
var sessionCommsSocket = connect_to_server(app);

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

app.ports.getStoredDesign.subscribe(({ uuidString }) => {
  get(uuidString, designStore).then((design) => {
    app.ports.setFocussedDesign.send({
      uuidString,
      design,
    });
  });
});

// Update design metrics for stored design
app.ports.updateDesignMetricsStatus.subscribe(
  ({ uuidString, updatedMetricsStatus }) => {
    get(uuidString, designStore).then((design) => {
      design.metricsJobStatus = updatedMetricsStatus;
      set(uuidString, design, designStore);
    });
  }
);

app.ports.deleteDesign.subscribe(({ uuidString }) => {
  del(uuidString, designStore);
});

app.ports.deleteAllDesigns.subscribe(() => {
  clear(designStore);
});
// }}}
// {{{ ReferenceSets

app.ports.storeReferenceSet.subscribe((uuidStringAndReferenceSet) => {
  const { uuidString, referenceSet } = uuidStringAndReferenceSet;
  if (idbAvailable) {
    set(uuidString, referenceSet, referenceSetStore);
  } else {
    console.log(
      "Storage is not available. IndexedDB must be enabled to store state."
    );
  }
});

app.ports.getReferenceSetForRefSetDetails.subscribe(({ uuidString }) => {
  get(uuidString, referenceSetStore).then((referenceSet) => {
    app.ports.setFocussedReferenceSet.send({
      uuidString,
      refSetValue: referenceSet,
    });
  });
});

app.ports.getReferenceSetForDesignDetails.subscribe(({ uuidString }) => {
  get(uuidString, referenceSetStore).then((referenceSet) => {
    app.ports.setSelectedRefSetDesignDetails.send({
      uuidString,
      refSetValue: referenceSet,
    });
  });
});

app.ports.deleteReferenceSet.subscribe(({ uuidString }) => {
  console.log("deleting: " + uuidString);
  del(uuidString, referenceSetStore);
});

// app.ports.deleteAllReferenceSets.subscribe(() => {
//   clear(referenceSetStore);
// });

// }}}
// {{{ Specifications

app.ports.storeSpecification.subscribe((uuidStringAndSpecification) => {
  const { uuidString, specification } = uuidStringAndSpecification;
  if (idbAvailable) {
    set(uuidString, specification, specificationStore);
  } else {
    console.log(
      "Storage is not available. IndexedDB must be enabled to store state."
    );
  }
});

app.ports.getSpecificationForSpecDetails.subscribe(({ uuidString }) => {
  get(uuidString, specificationStore).then((specification) => {
    app.ports.setFocussedSpecification.send({
      uuidString,
      specValue: specification,
    });
  });
});

app.ports.getSpecificationForDesignsPage.subscribe(({ uuidString }) => {
  get(uuidString, specificationStore).then((specification) => {
    app.ports.setSpecificationForDesign.send({
      uuidString,
      specification,
    });
  });
});

app.ports.getSpecificationForDesignDetails.subscribe(({ uuidString }) => {
  get(uuidString, specificationStore).then((specification) => {
    app.ports.setSelectedSpecDesignDetails.send({
      uuidString,
      specValue: specification,
    });
  });
});

app.ports.deleteSpecification.subscribe(({ uuidString }) => {
  del(uuidString, specificationStore);
});

// app.ports.deleteAllSpecifications.subscribe(() => {
//   clear(specificationStore);
// });

// }}}
// {{{ Session Comms

app.ports.outgoing.subscribe((action) => {
  sessionCommsSocket.send(JSON.stringify(action));
});

// }}}
// {{{ PV
app.ports.viewStructure.subscribe((pdbString) => {
  window.requestAnimationFrame(() => {
    const options = {
      width: "auto",
      height: "auto",
      antialias: true,
      quality: "medium",
    };
    var viewer = pv.Viewer(document.getElementById("viewer"), options);
    viewer.fitParent();
    var structure = pv.io.pdb(pdbString);
    viewer.fitTo(structure);
    viewer.trace("trace", structure, { color: pv.color.byChain() });
  });
});
// }}}
// {{{ VegaLite
app.ports.vegaPlot.subscribe((plotDetails) => {
  window.requestAnimationFrame(() => {
    vegaEmbed("#" + plotDetails.plotId, plotDetails.spec, {
      actions: false,
    }).catch(console.warn);
  });
});
// }}}
// }}}
