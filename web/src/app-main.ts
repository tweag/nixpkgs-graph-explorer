import { LitElement, css, html } from "lit";
import { customElement, queryAsync, state } from "lit/decorators.js";

import "@shoelace-style/shoelace/dist/themes/light.css";
import "@shoelace-style/shoelace/dist/components/icon/icon.js";
import "@shoelace-style/shoelace/dist/components/divider/divider.js";
import "@shoelace-style/shoelace/dist/components/drawer/drawer.js";
import SlDrawer from "@shoelace-style/shoelace/dist/components/drawer/drawer.js";

import type { ClickItemPayload } from "./nix-search";
import "./nix-search.ts";
import "./logos/graph-logo";

import dagre from "cytoscape-dagre";
import cytoscape from "cytoscape";

import { getGraph } from "./api";

import tweagLogo from "./assets/tweag-logo.svg";
import graphLogo from "./assets/line-chart.svg";

cytoscape.use(dagre);

function renderCyGraph(graphData: any, container: HTMLElement) {
  console.log(graphData);
  const data = graphData.cyto["graph-data"];
  cytoscape({
    container,
    ...data,
    layout: { name: "dagre" },
    style: [
      {
        selector: "node",
        style: {
          label: "data(id)",
        },
      },
    ],
  });
}

@customElement("app-main")
export class AppMain extends LitElement {
  @queryAsync("#cy") _cy: Promise<HTMLElement>;
  @queryAsync("#drawer-info") _drawer: Promise<SlDrawer>;
  @state() _error = false;

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

      <nix-search @click-item=${this.updateGraph}> </nix-search>
      <div id="cy-container">
        ${this._error
          ? html`<div>Error fetching graph data</div>`
          : html`<div id="cy"></div>`}
      </div>
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
        A
        <a href="https://tweag.io">Tweag</a>'s project.
        <div>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</div>
        <sl-button
          slot="footer"
          variant="primary"
          @click=${async () => (await this._drawer).hide()}
          >Close</sl-button
        >
      </sl-drawer>
    `;
  }

  private async updateGraph(ev: CustomEvent<ClickItemPayload>) {
    try {
      this._error = false; // Remove old errors
      const cy = await this._cy;
      const graphData = await getGraph(ev.detail.name);
      renderCyGraph(graphData, cy);
    } catch {
      this._error = true;
    }
  }

  static styles = css`
    :host {
      width: 100svw;
      height: 100svh;
      display: grid;
      gap: 10px;
      padding: 1rem;
      grid-template-columns: min(25%, 300px) 1fr;
      grid-template-rows: auto 1fr;
    }
    #cy-container {
      border: 1px solid var(--sl-panel-border-color);
    }
    #cy {
      width: 100%;
      height: 100%;
      left: 10px;
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
