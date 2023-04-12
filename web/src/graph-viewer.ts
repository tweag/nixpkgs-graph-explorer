import { LitElement, html, css } from "lit";
import type { TemplateResult } from "lit";
import { choose } from "lit/directives/choose.js";
import { customElement, property, queryAsync, state } from "lit/decorators.js";
import "@shoelace-style/shoelace/dist/components/tab-group/tab-group.js";
import "@shoelace-style/shoelace/dist/components/tab-panel/tab-panel.js";
import "@shoelace-style/shoelace/dist/components/tab/tab.js";

import dagre from "cytoscape-dagre";
import cytoscape from "cytoscape";

import { getGraph } from "./api";

cytoscape.use(dagre);

function renderCyGraph(graphData: any, container: HTMLElement) {
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

const TABS = {
  graph: "graph",
  raw: "raw",
} as const;

type ObjectValues<T> = T[keyof T];
type TabType = ObjectValues<typeof TABS>;
type TabEvData = { name: TabType };

@customElement("graph-viewer")
export class GraphViewer extends LitElement {
  @property() rawQuery?: string;
  @property() pkgName?: string;

  @state() _currentTab: TabType = "graph";
  @state() _error = false;

  @queryAsync("#cy") _cy: Promise<HTMLElement>;

  // Data from the API
  #graphData?: any;

  static styles = css`
    :host {
      height: 100%;
      display: grid;
      grid-template-rows: auto 1fr;
    }
    * {
      box-sizing: border-box;
    }
    #cy-container,
    #raw-data {
      border: 1px solid var(--sl-panel-border-color);
      border-top: none;
    }
    #cy {
      width: 100%;
      height: 100%;
      left: 10px;
    }
    #error {
      display: grid;
      place-items: center;
      height: 100%;
      font-family: Raleway, HelveticaNeue, "Helvetica Neue", Helvetica, Arial,
        sans-serif;
      font-size: 2rem;
    }

    sl-tab-panel {
      height: 100%;
      grid-area: body;
    }

    #raw-data {
      padding: 1em;
      font-family: monospace;
    }
  `;

  connectedCallback() {
    super.connectedCallback();
    this.addEventListener(
      "sl-tab-show",
      (ev: CustomEvent<TabEvData>) => (this._currentTab = ev.detail.name)
    );
  }

  render() {
    return html`
      <sl-tab-group>
        <sl-tab slot="nav" panel="${TABS.graph}">Graph</sl-tab>
        <sl-tab slot="nav" panel="${TABS.raw}">Text</sl-tab>
      </sl-tab-group>

      ${choose<TabType, TemplateResult>(this._currentTab, [
        [
          TABS.graph,
          () => html`
            <div id="cy-container">
              ${this._error
                ? html`<div id="error">Error fetching graph data :(</div>`
                : html`<div id="cy"></div>`}
            </div>
          `,
        ],
        [
          TABS.raw,
          () => html` <div id="raw-data">${this.#graphData.raw}</div> `,
        ],
      ])}
    `;
  }

  async updated(changedProperties: Map<string, any>) {
    if (
      this._error === false &&
      changedProperties.has("_currentTab") &&
      this._currentTab === TABS.graph
    ) {
      const cy = await this._cy;
      renderCyGraph(this.#graphData, cy);
    }

    if (this.pkgName != null && changedProperties.has("pkgName")) {
      try {
        this._error = false; // Remove old errors
        const cy = await this._cy;
        this.#graphData = await getGraph(this.pkgName);
        renderCyGraph(this.#graphData, cy);
      } catch {
        this._error = true;
      }
    }
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "graph-viewer": GraphViewer;
  }
}
