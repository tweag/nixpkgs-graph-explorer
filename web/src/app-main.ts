import { LitElement, css, html } from "lit";
import { customElement, queryAsync, state } from "lit/decorators.js";

import "@shoelace-style/shoelace/dist/themes/light.css";
import "@shoelace-style/shoelace/dist/components/icon/icon.js";
import "@shoelace-style/shoelace/dist/components/divider/divider.js";
import "@shoelace-style/shoelace/dist/components/drawer/drawer.js";
import SlDrawer from "@shoelace-style/shoelace/dist/components/drawer/drawer.js";
import "@shoelace-style/shoelace/dist/components/split-panel/split-panel.js";
import "@shoelace-style/shoelace/dist/components/tab-group/tab-group.js";
import "@shoelace-style/shoelace/dist/components/tab-panel/tab-panel.js";
import "@shoelace-style/shoelace/dist/components/tab/tab.js";

import type { QueryResultPayload } from "./api";

import "./nix-search";
import "./graph-viewer";
import "./code-editor";

import tweagLogo from "./assets/tweag-logo.svg";
import graphLogo from "./assets/line-chart.svg";

@customElement("app-main")
export class AppMain extends LitElement {
  @queryAsync("#drawer-info") _drawer: Promise<SlDrawer>;
  @state() _queryResult?: QueryResultPayload;

  private setQueryResult(ev: CustomEvent<QueryResultPayload>) {
    this._queryResult = ev.detail;
  }

  render() {
    const year = new Date().getFullYear();
    return html`
      <header>
        <img src=${graphLogo} class="logo graph-logo" />
        <h3>Nixpkgs Graph Explorer</h3>
        <a href="https://www.tweag.io/">
          <img src=${tweagLogo} class="logo tweag" alt="Tweag logo" />
        </a>
      </header>

      <sl-tab-group>
        <sl-tab slot="nav" panel="search">Search</sl-tab>
        <sl-tab slot="nav" panel="query">Query</sl-tab>

        <sl-tab-panel name="search">
          <nix-search @query-result=${this.setQueryResult}> </nix-search>
        </sl-tab-panel>
        <sl-tab-panel name="query">
          <code-editor @query-result=${this.setQueryResult}> </code-editor>
        </sl-tab-panel>
      </sl-tab-group>

      <graph-viewer slot="end" .queryResult=${this._queryResult}>
      </graph-viewer>

      <footer>
        <div>
          © ${year}
          <a href="https://github.com/tweag/nixpkgs-graph-explorer/contributors"
            >Nixpkgs graph explorer</a
          >
          Contributors.
          <div>All rights reserved.</div>
        </div>

        <div @click=${async () => (await this._drawer).show()} id="info">
          <sl-icon slot="icon" name="info-circle-fill"> info </sl-icon>
          Info
        </div>

        <sl-divider vertical></sl-divider>

        <span>
          Nixpkgs rev:
          <a
            href="https://github.com/NixOS/nixpkgs/tree/53dad94e874c9586e71decf82d972dfb640ef044"
          >
            53dad94
          </a>
        </span>

        <sl-divider vertical></sl-divider>

        <a href="https://docs.janusgraph.org/getting-started/gremlin/">
          Gremlin Query Language
        </a>

        <sl-divider vertical></sl-divider>

        <a href="https://github.com/tweag/nixpkgs-graph-explorer">
          <sl-icon slot="icon" name="github" library="default"> </sl-icon>
        </a>
      </footer>

      <sl-drawer label="About Nixpkgs Graph Explorer" id="drawer-info">
        ${aboutText}

        <sl-button
          slot="footer"
          variant="primary"
          @click=${async () => (await this._drawer).hide()}
        >
          Close
        </sl-button>
      </sl-drawer>
    `;
  }

  static styles = css`
    :host {
      width: 100svw;
      height: 100svh;
      display: grid;
      gap: 10px;
      padding: 1rem;
      grid-template-columns: min(25%, 300px) 1fr;
      grid-template-rows: auto minmax(0, 1fr);
    }

    header,
    footer {
      grid-column: 1 / -1;
      color: black;
      display: flex;
      align-items: center;
      font-family: Raleway, HelveticaNeue, "Helvetica Neue", Helvetica, Arial,
        sans-serif;
    }
    header > *:nth-child(1) {
      width: 2rem;
      height: 2rem;
    }
    header > :nth-child(3) {
      margin-left: auto;
    }

    footer > :first-child {
      margin-right: auto;
    }
    footer {
      gap: 1rem;
      align-items: center;
      font-family: var(--sl-font-sans);
      font-size: var(--sl-font-size-small);
      font-weight: var(--sl-font-weight-semibold);
      color: var(--sl-color-neutral-600);
      line-height: var(--sl-line-height-normal);
      white-space: nowrap;
    }
    a {
      color: var(--sl-color-primary-600);
    }
    sl-icon[name="github"] {
      width: 1.75rem;
      height: 1.75rem;
    }
    sl-drawer {
      --size: 50vw;
      display: flex;
      flex-direction: column;
      font-family: Raleway, HelveticaNeue, "Helvetica Neue", Helvetica, Arial,
        sans-serif;
    }

    #info {
      cursor: pointer;
      display: flex;
      color: var(--sl-color-primary-600);
      align-items: center;
      gap: 0.5rem;
    }
  `;
}

declare global {
  interface HTMLElementTagNameMap {
    "app-main": AppMain;
  }
}

const aboutText = html`
  <p>
    <code>nixpkgs-graph-explorer</code> is a project started at
    <a href="https://tweag.io">Tweag</a>. It aims at making
    <code>nixpkgs</code> more visual and discoverable.
  </p>
  <p>
    Thanks to <code>nixpkgs-graph-explorer</code>, one can
    <span>
      <ul>
        <li>search for a package and find its dependencies</li>
        <li>analyze relationships between packages</li>
      </ul>
    </span>
  </p>
  <h3>How to use?</h3>
  <p>
    <span>
      <ul>
        <li>Go to the left panel and search for a package.</li>
        <li>A list of results will be displayed.</li>
        <li>Click on the result you're interested in.</li>
        <li>Explore!</li>
      </ul>
    </span>
  </p>
`;
