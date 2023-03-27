import { LitElement, css, html } from "lit";
import { customElement, query } from "lit/decorators.js";
import "@shoelace-style/shoelace/dist/themes/light.css";
import "./nix-search.ts";

import cytoscape from "cytoscape";
import dagre from "cytoscape-dagre";

const API_URL = "/api";

interface GraphEv {
  name: string;
}

async function getGraph(pkgName: string, container: HTMLElement) {
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
  const r = await response.json();
  console.log(r);
  const data = r.cyto["graph-data"];
  cytoscape.use(dagre);
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
  @query("#cy", true) _cy!: HTMLElement;
  render() {
    return html`
      <nix-search @graph=${this.updateGraph}> </nix-search>
      <div id="cy-container">
        <div id="cy"></div>
      </div>
    `;
  }

  private updateGraph(ev: CustomEvent<GraphEv>) {
    getGraph(ev.detail.name, this._cy);
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
