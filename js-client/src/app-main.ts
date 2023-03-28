import { LitElement, css, html } from "lit";
import { customElement, queryAsync, state } from "lit/decorators.js";
import "@shoelace-style/shoelace/dist/themes/light.css";
import type { ClickItemPayload } from "./nix-search";
import "./nix-search.ts";

import dagre from "cytoscape-dagre";
import cytoscape from "cytoscape";

const API_URL = __API_URL__;
cytoscape.use(dagre);

async function getGraph(pkgName: string) {
  const response = await fetch(`${API_URL}/gremlin`, {
    method: "POST",
    headers: {
      Accept: "application/json",
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      query: `g.V().filter{it.get().value('pname').matches('${pkgName}')}.repeat(outE().otherV().simplePath()).until(__.not(outE().simplePath())).path().by('pname').by(label)`,
    }),
  });
  return await response.json();
}

function renderCyGraph(graphData: any, container: HTMLElement) {
  console.log(graphData);
  const data = graphData.cyto["graph-data"];
  cytoscape({
    container,
    layout: { name: "dagre" },
    ...data,
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
  @state() _error = false;
  render() {
    return html`
      <nix-search @click-item=${this.updateGraph}> </nix-search>
      <div id="cy-container">
        ${this._error
          ? html`<div>Error fetching graph data</div>`
          : html`<div id="cy"></div>`}
      </div>
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
      padding: 2rem;
      grid-template-columns: 200px 1fr;
    }
    #cy-container {
      border: 1px solid var(--sl-panel-border-color);
    }
    #cy {
      width: 100%;
      height: 100%;
      left: 10px;
    }
  `;
}

declare global {
  interface HTMLElementTagNameMap {
    "app-main": AppMain;
  }
}
