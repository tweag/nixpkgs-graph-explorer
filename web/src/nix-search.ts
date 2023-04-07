import {LitElement, css, html} from "lit";
import {customElement, state} from "lit/decorators.js";
import "@shoelace-style/shoelace/dist/themes/light.css";
import "@shoelace-style/shoelace/dist/components/button/button.js";
import "@shoelace-style/shoelace/dist/components/menu/menu.js";
import "@shoelace-style/shoelace/dist/components/menu-item/menu-item.js";
import "@shoelace-style/shoelace/dist/components/input/input.js";
import "@shoelace-style/shoelace/dist/components/spinner/spinner.js";
import "@shoelace-style/shoelace/dist/components/alert/alert.js";
import "@shoelace-style/shoelace/dist/components/icon/icon.js";
import {classMap} from "lit/directives/class-map.js";
import {getPackages} from "./api";
import type {Cursor, Pkg} from "./api";

export interface ClickItemPayload {
  name: string;
}

type EventInput = Event & {target: HTMLInputElement};

// TODO Add pagination
@customElement("nix-search")
export class NixSearch extends LitElement {
  @state()
  pkgs: Pkg[] = [];

  @state()
  cursor?: Cursor;

  @state()
  loading = true;

  @state()
  selectedPkg?: string;

  searchInput = "";

  #timer: ReturnType<typeof setTimeout>;

  constructor() {
    super();
    // trigger a request to get an initial list of packages displayed
    this.getPackages();
  }

  async getPackages(search = "", limit = 10) {
    const {packages, new_cursor} = await getPackages({search, limit});
    this.pkgs = packages;
    this.cursor = new_cursor;
    this.loading = false;
  }

  updateSearchQuery(ev: EventInput) {
    this.loading = true;
    this.searchInput = ev.target.value;

    // Debounce API requests
    clearTimeout(this.#timer);
    this.#timer = setTimeout(() => {
      this.getPackages(this.searchInput);
    }, 250);
  }

  render() {
    return html`
      <sl-input
        @input=${this.updateSearchQuery}
        placeholder="Search by package name..."
        clearable
      >
      </sl-input>
      ${this.loading
        ? html`
        <div style="margin-top: 3em; width: 100%; text-align: center;">
          <sl-spinner
            style="font-size: 50px; --track-width: 10px;"
          ></sl-spinner>
        </div>`
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
      ({pname}) =>
        html`
              <sl-menu-item
                @click=${this.clickPackageHandler}
                value=${pname}
                class=${classMap({selected: this.selectedPkg === pname})}
                >${pname}</sl-menu-item
              >
            `
    )}
      </sl-menu>
    `;
  }

  private clickPackageHandler(ev: EventInput) {
    const name = ev.target.value.trim();
    this.selectedPkg = name;

    if (name) {
      const options = {
        detail: {name},
        bubbles: true,
        composed: true,
      };
      this.dispatchEvent(
        new CustomEvent<ClickItemPayload>("click-item", options)
      );
    }
  }

  static styles = css`
    :host {
    }
    sl-input {
      margin-bottom: 1em;
    }
    .selected {
      background-color: var(--sl-color-primary-200);
    }
  `;
}

declare global {
  interface HTMLElementTagNameMap {
    "nix-search": NixSearch;
  }
}
