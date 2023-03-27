import { LitElement, css, html } from "lit";
import { customElement, state } from "lit/decorators.js";
import "@shoelace-style/shoelace/dist/themes/light.css";
import "@shoelace-style/shoelace/dist/components/button/button.js";
import "@shoelace-style/shoelace/dist/components/menu/menu.js";
import "@shoelace-style/shoelace/dist/components/menu-item/menu-item.js";
import "@shoelace-style/shoelace/dist/components/input/input.js";
import "@shoelace-style/shoelace/dist/components/spinner/spinner.js";
import "@shoelace-style/shoelace/dist/components/alert/alert.js";
import "@shoelace-style/shoelace/dist/components/icon/icon.js";

interface Operation {
  type: "search" | "graph";
  data: string[];
}

type EventInput = Event & { target: HTMLInputElement };

@customElement("nix-search")
export class NixSearch extends LitElement {
  worker = new Worker(new URL("./packages", import.meta.url), {
    type: "module",
  });

  @state()
  pkgs: string[] = [];

  @state()
  loading = true;

  constructor() {
    super();
    this.worker.addEventListener("message", (msg: MessageEvent<Operation>) => {
      const { type, data } = msg.data;
      if (type === "search") {
        this.pkgs = data;
        this.loading = false;
      }
    });
    this.worker.postMessage({ type: "search", data: "" });
  }

  updateValue(ev: EventInput) {
    this.loading = true;
    this.worker.postMessage({ type: "search", data: ev.target.value });
  }

  render() {
    return html`
      <sl-input
        @input=${this.updateValue}
        placeholder="Search a package"
        clearable
      >
      </sl-input>
      ${this.loading
        ? html`<sl-spinner
            style="font-size: 50px; --track-width: 10px;"
          ></sl-spinner>`
        : this.renderTable()}
    `;
  }

  renderTable() {
    if (this.pkgs.length === 0)
      return html`<sl-alert open>
        <sl-icon slot="icon" name="info-circle"></sl-icon>
        No results
      </sl-alert> `;
    return html`
      <sl-menu>
        ${this.pkgs.map(
          (pkg) =>
            html`
              <sl-menu-item @click=${this.updateGraph} value=${pkg}
                >${pkg}</sl-menu-item
              >
            `
        )}
      </sl-menu>
    `;
  }

  private updateGraph(ev: EventInput) {
    const name = ev.target.value.trim();
    if (name) {
      const options = {
        detail: { name },
        bubbles: true,
        composed: true,
      };
      this.dispatchEvent(new CustomEvent("graph", options));
    }
  }

  static styles = css`
    :host {
    }
    sl-input {
      margin-bottom: 1em;
    }
  `;
}

declare global {
  interface HTMLElementTagNameMap {
    "nix-search": NixSearch;
  }
}
