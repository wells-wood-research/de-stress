import { Stage } from "ngl";

class NGLViewer extends HTMLElement {
  constructor() {
    super();
  }
  connectedCallback() {
    this.showStructure();
  }
  // attributeChangedCallback() {
  //   this.showStructure();
  // }
  // static get observedAttributes() {
  //   return ["pdb-string"];
  // }

  showStructure() {
    const pdbString = this.getAttribute("pdb-string");

    var stage = new Stage(this.id, { backgroundColor: "#BEBEBE" });
    var stringBlob = new Blob([pdbString], { type: "text/plain" });
    stage.loadFile(stringBlob, { ext: "pdb" }).then(function (component) {
      component.addRepresentation("cartoon");
      component.autoView();
    });
  }
}

customElements.define("ngl-viewer", NGLViewer);
