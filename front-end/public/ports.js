function connect_to_server(app) {
  window.sessionCommsSocket = new WebSocket("ws://localhost:8181/app-comms");
  sessionCommsSocket.onopen = function() {
    // subscribe to some channels
    console.log("Established connection to server.");
  };

  sessionCommsSocket.onmessage = function(event) {
    app.ports.webSocketIncoming.send(JSON.parse(event.data));
  };

  sessionCommsSocket.onclose = function(event) {
    console.log(
      "Socket is closed. Reconnect will be attempted in 1 second.",
      event.reason
    );
    setTimeout(function() {
      connect_to_server(app);
    }, 1000);
  };

  sessionCommsSocket.onerror = function(err) {
    console.error("Socket encountered error: ", err.message, "Closing socket");
    sessionCommsSocket.close();
  };
}

// On load, listen to Elm!
window.addEventListener("load", (_) => {
  window.sessionCommsSocket = new WebSocket("ws://localhost:8181/app-comms");
  window.ports = {
    init: (app) => {
      app.ports.outgoing.subscribe(({ action, data }) => {
        actions[action]
          ? actions[action](app, data)
          : console.warn(`I didn't recognize action "${action}".`);
      });

      // Session comms using WebSockets
      connect_to_server(app);
      app.ports.webSocketOutgoing.subscribe((action) => {
        window.sessionCommsSocket.send(JSON.stringify(action));
      });

      // Vega Lite
      // Plots a vega-lite specification created in Elm
      app.ports.vegaPlot.subscribe((plotDetails) => {
        window.requestAnimationFrame(() => {
          vegaEmbed("#" + plotDetails.plotId, plotDetails.spec, {
            actions: false,
          }).catch(console.warn);
        });
      });
    },
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

// Update design metrics for stored design
const updateMetricsJobStatus = (_, metricsAndKey) => {
  var { storeKey, metricsJobStatus } = metricsAndKey;
  idbKeyval.get(storeKey, designStore).then((design) => {
    design.metricsJobStatus = metricsJobStatus;
    idbKeyval.set(storeKey, design, designStore);
  });
};

// Get design
const getDesign = (app, storeKey) => {
  idbKeyval.get(storeKey, designStore).then((design) => {
    app.ports.setFocussedDesign.send({
      uuidString: storeKey,
      design: design,
    });
  });
};

// Delete design
const deleteDesign = (_, storeKey) => {
  idbKeyval.del(storeKey, designStore);
};

// View Structure
const viewStructure = (_, pdbString) => {
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
};

// }}}
// {{{ Reference Sets
//
const referenceSetStore = new idbKeyval.Store(
  "reference-sets",
  "reference-set-store"
);

// Store reference set
const storeReferenceSet = (_, referenceSetAndKey) => {
  var { storeKey, referenceSet } = referenceSetAndKey;
  if (idbAvailable) {
    idbKeyval.set(storeKey, referenceSet, referenceSetStore);
  } else {
    console.log(
      "Storage is not available. IndexedDB must be enabled to store state."
    );
  }
};

// Get referenceSet
const getReferenceSetForDesign = (app, storeKey) => {
  idbKeyval.get(storeKey, referenceSetStore).then((referenceSet) => {
    app.ports.referenceSetForDesign.send({
      uuidString: storeKey,
      referenceSet: referenceSet,
    });
  });
};

// Delete referenceSet
const deleteReferenceSet = (_, storeKey) => {
  idbKeyval.del(storeKey, referenceSetStore);
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
  idbKeyval.get(storeKey, specificationStore).then((specification) => {
    app.ports.setFocussedSpecification.send({
      uuidString: storeKey,
      specification: specification,
    });
  });
};

// Get specification for design page
const getSpecificationForDesign = (app, storeKey) => {
  idbKeyval.get(storeKey, specificationStore).then((specification) => {
    app.ports.specificationForDesign.send({
      uuidString: storeKey,
      specification: specification,
    });
  });
};

// Get specification for designs page
const getSpecificationForDesignsPage = (app, storeKey) => {
  idbKeyval.get(storeKey, specificationStore).then((specification) => {
    app.ports.specificationForDesignsPage.send({
      uuidString: storeKey,
      specification: specification,
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
  UPDATE_METRICS_STATUS: updateMetricsJobStatus,
  GET_DESIGN: getDesign,
  DELETE_DESIGN: deleteDesign,
  VIEW_STRUCTURE: viewStructure,
  // Reference Sets
  STORE_REFERENCE_SET: storeReferenceSet,
  GET_REFERENCE_SET_FOR_DESIGN: getReferenceSetForDesign,
  DELETE_REFERENCE_SET: deleteReferenceSet,
  // Specifications
  STORE_SPECIFICATION: storeSpecification,
  GET_SPECIFICATION: getSpecification,
  GET_SPECIFICATION_FOR_DESIGN: getSpecificationForDesign,
  GET_SPECIFICATION_FOR_DESIGNS_PAGE: getSpecificationForDesignsPage,
  DELETE_SPECIFICATION: deleteSpecification,
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
