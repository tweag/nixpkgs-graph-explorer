import { LitElement, html, css } from "lit";
import type { TemplateResult } from "lit";
import { choose } from "lit/directives/choose.js";
import { customElement, property, queryAsync, state } from "lit/decorators.js";
import "@shoelace-style/shoelace/dist/components/tab-group/tab-group.js";
import "@shoelace-style/shoelace/dist/components/tab/tab.js";

import dagre from "cytoscape-dagre";
import cytoscape from "cytoscape";

import { DataPayload, QueryResultPayload } from "./api";

cytoscape.use(dagre);

function renderCyGraph(result: QueryResultPayload, container: HTMLElement) {
  const graphData = result?.data;
  if (graphData == null) return;
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
  @property() queryResult?: QueryResultPayload;

  @state() _currentTab: TabType = TABS.graph;
  @state() _errorMsg?: string;

  @queryAsync("#cy") _cy: Promise<HTMLElement>;

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
      grid-row: 1 / -1;
      height: 100%;
      font-family: Raleway, HelveticaNeue, "Helvetica Neue", Helvetica, Arial,
        sans-serif;
      font-size: 2rem;
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
    if (this.queryResult == null) return null;

    if (this._errorMsg != null)
      return html`<div id="error">${this._errorMsg}</div>`;

    return html`
      <sl-tab-group>
        <sl-tab
          slot="nav"
          panel="${TABS.graph}"
          ?active=${this._currentTab === TABS.graph}
          >Graph</sl-tab
        >
        <sl-tab
          slot="nav"
          panel="${TABS.raw}"
          ?active=${this._currentTab === TABS.raw}
          >Text</sl-tab
        >
      </sl-tab-group>

      ${choose<TabType, TemplateResult>(this._currentTab, [
        [
          TABS.graph,
          () => html`
            <div id="cy-container">
              <div id="cy"></div>
            </div>
          `,
        ],
        [
          TABS.raw,
          () => html`<div id="raw-data">${this.queryResult.data.raw}</div>`,
        ],
      ])}
    `;
  }

  async updated(changedProperties: Map<string, any>) {
    if (
      this._errorMsg != null &&
      changedProperties.has("_currentTab") &&
      this._currentTab === TABS.graph
    ) {
      const cy = await this._cy;
      renderCyGraph(this.queryResult, cy);
    }

    if (this.queryResult != null && changedProperties.has("queryResult")) {
      // Server error
      if (this.queryResult.error === true) {
        this._errorMsg = "Error fetching graph data :(";
        return;
      }
      // Empty results
      if (emptyQueryResult(this.queryResult.data)) {
        this._errorMsg = "No results :(";
        return;
      }

      try {
        this._errorMsg = null; // Remove old errors
        const cy = await this._cy;
        if (this._currentTab === TABS.graph)
          renderCyGraph(this.queryResult, cy);
      } catch {
        this._errorMsg = "Error";
      }
    }
  }
}

function emptyQueryResult(data: DataPayload) {
  if (data?.raw == null) return true;
  if (data.raw.length === 0) return true;
  if (data.raw === "[]") return true;
  return false;
}

declare global {
  interface HTMLElementTagNameMap {
    "graph-viewer": GraphViewer;
  }
}
