import { LitElement, html, css } from "lit";
import type { TemplateResult } from "lit";
import { choose } from "lit/directives/choose.js";
import { customElement, property, queryAsync, state } from "lit/decorators.js";
import "@shoelace-style/shoelace/dist/components/tab-group/tab-group.js";
import "@shoelace-style/shoelace/dist/components/tab/tab.js";
import "@shoelace-style/shoelace/dist/components/tab/tab.js";
import "@shoelace-style/shoelace/dist/components/tree/tree.js";
import "@shoelace-style/shoelace/dist/components/tree-item/tree-item.js";
import "@shoelace-style/shoelace/dist/components/split-panel/split-panel.js";
import "@shoelace-style/shoelace/dist/components/icon/icon.js";
import "@shoelace-style/shoelace/dist/components/details/details.js";

import dagre from "cytoscape-dagre";
import cytoscape from "cytoscape";

import { DataPayload, QueryResultPayload } from "./api";

cytoscape.use(dagre);

function renderCyGraph(result: QueryResultPayload, container: HTMLElement) {
  const graphData = result?.data;
  if (graphData == null) return;
  const data = graphData.cyto["graph-data"];
  const cy = cytoscape({
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
  cy.on("select", "node", function (graphEvent) {
    const customEvent = new CustomEvent("node-selected", {
      detail: { packageName: graphEvent.target.id() },
      bubbles: true,
      composed: true,
    });
    container.dispatchEvent(customEvent);
  });
  cy.on("unselect", "node", function (graphEvent) {
    const customEvent = new CustomEvent("node-deselected", {
      detail: { packageName: graphEvent.target.id() },
      bubbles: true,
      composed: true,
    });
    container.dispatchEvent(customEvent);
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

  @state() _selectedNodes: Array<string> = [];

  // Current error friendly message. The absence of a value (null/undefined)
  // represents the lack of a current error
  @state() _errorMsg?: TemplateResult | string;

  @queryAsync("#cy") _cy: Promise<HTMLElement>;

  static styles = css`
    :host {
      display: flex;
      flex-direction: column;
    }

    #cy-container {
      max-height: 100%;
      overflow: hidden;
    }

    #cy {
      width: 100%;
      height: 100%;
    }

    #error {
      display: grid;
      place-items: center;
      grid-row: 1 / -1;
      height: 100%;
      font-family: var(--sl-font-sans);
      font-size: var(--sl-font-size-2x-large);
    }

    #raw-data {
      padding: var(--sl-spacing-small);
      font-family: var(--sl-font-mono);
    }

    #selected-node-details {
      font-family: var(--sl-font-sans);
      color: var(--sl-color-neutral-800);
      line-height: var(--sl-line-height-normal);
      background: var(--sl-color-neutral-100);
      overflow-y: scroll;
    }

    .tab-content {
      border: 1px solid var(--sl-panel-border-color);
      border-top: none;
      flex-grow: 1;
      overflow: hidden;
    }

    sl-tab-group {
      flex-shrink: 0;
      flex-grow: 0;
    }

    sl-split-panel::part(divider) {
      background-color: var(--sl-color-neutral-200);
    }

    sl-icon {
      position: absolute;
      border-radius: var(--sl-border-radius-small);
      background: var(--sl-color-neutral-300);
      color: var(--sl-color-neutral-0);
      padding: 0.5rem 0.125rem;
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
            <sl-split-panel class="tab-content" position="75">
              <sl-icon slot="divider" name="grip-vertical"></sl-icon>
              <div
                id="cy-container"
                slot="start"
                @node-selected=${this._handleSelectedNode}
                @node-deselected=${this._handleDeselectedNode}
              >
                <div id="cy"></div>
              </div>
              <div id="selected-node-details" slot="end">
                <packages-outline .packages=${this._selectedNodes}></packages-outline>
              </div>
              </div>
            </sl-split-panel>
          `,
        ],
        [
          TABS.raw,
          () =>
            html`<div class="tab-content" id="raw-data">
              ${this.queryResult.data.raw}
            </div>`,
        ],
      ])}
    `;
  }

  private _handleSelectedNode(event: CustomEvent) {
    this._selectedNodes = [event.detail.packageName, ...this._selectedNodes];
  }

  private _handleDeselectedNode(event: CustomEvent) {
    this._selectedNodes = this._selectedNodes.filter(
      (name) => name !== event.detail.packageName
    );
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
      this._selectedNodes = [];

      // Server error
      if (this.queryResult.error === true) {
        this._errorMsg = html`Error fetching graph data :(<br />
          Please check your query and try again later`;
        return;
      }
      // Empty results
      if (emptyQueryResult(this.queryResult.data)) {
        this._errorMsg = html`No results :(<br />Please change your query`;
        return;
      }

      try {
        this._errorMsg = null; // Remove old errors
        const cy = await this._cy;
        if (this._currentTab === TABS.graph)
          renderCyGraph(this.queryResult, cy);
      } catch {
        this._errorMsg = html`Something went wrong :(<br />
          Please try again later, if the error persists contact us on GitHub`;
      }
    }
  }
}

@customElement("packages-outline")
export class PackagesOutline extends LitElement {
  // TODO Eventually, this should be a collection of Pkg and contain actual details.
  @property({ attribute: false }) packages: Array<string> = [];

  static styles = css`
    sl-details {
      margin: var(--sl-spacing-small);
      margin-left: var(--sl-spacing-medium);
    }
  `;

  render() {
    return this.packages.map(
      (packageName, index) => html`
        <sl-details summary="${packageName}" ?open=${index === 0}>
          Details about ${packageName}.
        </sl-details>
      `
    );
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
