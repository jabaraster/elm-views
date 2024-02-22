customElements.define(
  "modal-dialog",
  class extends HTMLElement {
    static observedAttributes = ["open"];
    #isConnected = false;
    connectedCallback() {
      this.#setContent();
      this.#isConnected = true;
    }
    attributeChangedCallback() {
      if (this.#isConnected) this.#setContent();
    }
    #setContent() {
      const isOpen = this.getAttribute("open") !== null;
      const dialog = this.querySelector("dialog");
      if (dialog) {
        if (isOpen) {
          dialog.onclick = (event) => {
            // closing only if clicked on scrim, not on the filler
            // if (event.currentTarget === event.target) dialog.close();
          };
          dialog.showModal();
        } else {
          dialog.close();
        }
      }
    }
  },
);
