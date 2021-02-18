function localizeDate(lang, year, month) {
  const dateTimeFormat = new Intl.DateTimeFormat(lang, {
    weekday: "long",
    year: "numeric",
    month: "long",
    day: "numeric",
  });

  return dateTimeFormat.format(new Date(year, month));
}

class IntlDate extends HTMLElement {
  // things required by Custom Elements
  constructor() {
    super();
  }
  connectedCallback() {
    this.setTextContent();
  }
  attributeChangedCallback() {
    this.setTextContent();
  }
  static get observedAttributes() {
    return ["lang", "year", "month"];
  }

  // Our function to set the textContent based on attributes.
  setTextContent() {
    const lang = this.getAttribute("lang");
    const year = this.getAttribute("year");
    const month = this.getAttribute("month");
    this.textContent = localizeDate(lang, year, month);
  }
}

export { IntlDate };
