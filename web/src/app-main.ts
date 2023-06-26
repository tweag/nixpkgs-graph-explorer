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
        <div class="logo-group">
          <img src=${graphLogo} class="logo graph-logo" />
          <h1 class="logo-title">Nixpkgs Graph Explorer</h1>
        </div>
        <a href="https://www.tweag.io/">
          <img src=${tweagLogo} class="logo tweag" alt="Tweag logo" />
        </a>
      </header>

      <main>
        <nix-search @query-result=${this.setQueryResult}> </nix-search>
        <graph-viewer slot="end" .queryResult=${this._queryResult}>
        </graph-viewer>
      </main>

      <footer>
        <div>
          © ${year}
          <a href="https://github.com/tweag/nixpkgs-graph-explorer/contributors"
            >Nixpkgs graph explorer</a
          >
          Contributors.
          <div>All rights reserved.</div>
        </div>

        <div class="footer-right">
          <div @click=${async () => (await this._drawer).show()} id="info">
            <sl-icon slot="icon" name="info-circle-fill"> info </sl-icon>
            Info
          </div>

          <sl-divider vertical></sl-divider>

          <div>
            Nixpkgs rev:
            <a
              href="https://github.com/NixOS/nixpkgs/tree/53dad94e874c9586e71decf82d972dfb640ef044"
            >
              53dad94
            </a>
          </div>

          <sl-divider vertical></sl-divider>

          <a href="https://docs.janusgraph.org/getting-started/gremlin/">
            Gremlin Query Language
          </a>

          <sl-divider vertical></sl-divider>

          <a href="https://github.com/tweag/nixpkgs-graph-explorer">
            <sl-icon slot="icon" name="github" library="default"> </sl-icon>
          </a>
        </div>
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
      display: flex;
      flex-direction: column;
      margin: 0;
      padding: 0;
      font-family: var(--sl-font-sans);
    }

    a {
      color: var(--sl-color-primary-600);
    }

    a img {
      display: block;
    }

    header,
    footer {
      display: flex;
      align-items: center;
      flex-shrink: 0;
      flex-grow: 0;
      justify-content: space-between;
      padding: var(--sl-spacing-small);
      white-space: nowrap;
      overflow: hidden;
    }

    .logo-group {
      display: flex;
      flex-wrap: nowrap;
      align-items: center;
      gap: var(--sl-spacing-x-small);
    }
    .logo-title {
      font-weight: var(--sl-font-weight-bold);
      font-size: var(--sl-font-size-large);
    }
    .graph-logo {
      width: 2rem;
      height: 2rem;
    }

    main {
      overflow: hidden;
      flex-grow: 1;
      display: flex;
      gap: var(--sl-spacing-x-small);
    }

    main nix-search {
      flex-grow: 0;
      flex-shrink: 0;
      width: min(40%, 40rem);
      padding-left: var(--sl-spacing-small);
    }

    main graph-viewer {
      flex-grow: 1;
      padding-right: var(--sl-spacing-small);
    }

    footer {
      font-family: var(--sl-font-sans);
      font-size: var(--sl-font-size-small);
      font-weight: var(--sl-font-weight-semibold);
      color: var(--sl-color-neutral-600);
      line-height: var(--sl-line-height-normal);
    }
    footer sl-icon[name="github"] {
      width: 1.75rem;
      height: 1.75rem;
    }
    footer .footer-right {
      display: flex;
      align-items: center;
      gap: var(--sl-spacing-x-small);
      margin-left: var(--sl-spacing-small);
    }
    footer .footer-right sl-divider {
      height: 3em;
      margin: var(--sl-spacing-small);
    }
    @media only screen and (max-width: 50rem) {
      footer .footer-right :not(sl-icon) {
        display: none;
      }
    }

    #info {
      cursor: pointer;
      display: flex;
      color: var(--sl-color-primary-600);
      align-items: center;
      gap: 0.5rem;
    }

    sl-drawer {
      --size: 50vw;
      flex-direction: column;
      font-family: var(--sl-font-sans);
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
