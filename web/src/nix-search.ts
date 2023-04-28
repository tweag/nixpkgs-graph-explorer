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
import { classMap } from "lit/directives/class-map.js";
import { getGraph, getPackages } from "./api";
import type { Cursor, Pkg, QueryResultPayload } from "./api";
import { assign, createMachine, interpret } from "xstate";

type EventInput = Event & { target: HTMLInputElement };

type Context = {
  pkgs: Pkg[];
  selectedPkg?: string;
  input: string;
  cursor?: Cursor;
  limit: number;
  cursors: Cursor[];
  cursor_position: number;
};

type InputEvent = { type: "INPUT"; input: string };

type QueryDone = {
  type: "done.invoke.queryPkgs";
  data: Pick<Context, "pkgs" | "cursor">;
};

type SearchEvent = InputEvent | QueryDone | { type: "NEXT" } | { type: "PREV" };

const searchMachine = createMachine<Context, SearchEvent>({
  initial: "loading",
  context: {
    pkgs: [],
    input: "",
    limit: 10,
    cursors: [],
    cursor_position: -1,
  },
  states: {
    loading: {
      on: {
        INPUT: { target: "debouncing", actions: "saveInput" },
      },
      invoke: {
        id: "queryPkgs",
        src: "queryPkgs",
        onDone: {
          target: "waiting",
          actions: assign((ctx, event: QueryDone) => {
            const { pkgs, cursor } = event.data;
            return {
              pkgs,
              cursors: cursor ? [...ctx.cursors, cursor] : ctx.cursors,
            };
          }),
        },
        onError: {
          target: "error",
        },
      },
    },

    waiting: {
      on: {
        INPUT: { target: "debouncing", actions: "saveInput" },
        NEXT: {
          target: "loading",
          cond: "hasNextCursor",
          actions: assign({
            cursor_position: (ctx) => ctx.cursor_position + 1,
          }),
        },
        PREV: {
          target: "loading",
          cond: "hasPrevCursor",
          actions: assign({
            cursor_position: (ctx) => ctx.cursor_position - 1,
          }),
        },
      },
    },

    error: {
      on: {
        INPUT: { target: "debouncing", actions: "saveInput" },
      },
    },

    debouncing: {
      on: {
        INPUT: { target: "debouncing", actions: "saveInput" },
      },
      after: {
        250: { target: "loading" },
      },
    },
  },
});

function hasNextCursor({ cursor_position, cursors }: Partial<Context>) {
  const last_cursor_index = cursors.length - 1;
  return cursor_position < last_cursor_index;
}

function hasPrevCursor(ctx: Context) {
  return ctx.cursor_position >= 0;
}

async function queryPkgs({ input, limit, cursors, cursor_position }: Context) {
  const result = await getPackages({
    search: input,
    limit,
    cursor: cursor_position === -1 ? null : cursors[cursor_position],
  });
  return {
    pkgs: result.packages,
    // Add only new cursors
    cursor: hasNextCursor({ cursors, cursor_position })
      ? null
      : result.new_cursor,
  };
}

const saveInput = assign((_, event: InputEvent) => ({
  input: event.input,
  cursors: [],
  cursor_position: -1,
}));

@customElement("nix-search")
export class NixSearch extends LitElement {
  private searchMachine = interpret(
    searchMachine.withConfig({
      guards: { hasPrevCursor, hasNextCursor },
      services: { queryPkgs },
      actions: { saveInput },
    })
  )
    .onTransition(() => this.requestUpdate())
    .start();

  @state()
  selectedPkg?: string;

  updateSearchQuery(ev: EventInput) {
    this.searchMachine.send({ type: "INPUT", input: ev.target.value });
  }

  isLoading() {
    const state = this.searchMachine.getSnapshot().value;
    return state !== "waiting" && state !== "error";
  }

  render() {
    return html`
      <sl-input
        @input=${this.updateSearchQuery}
        placeholder="Search by package name..."
        clearable
      >
      </sl-input>
      ${this.isLoading()
        ? html`<div class="spinner-holder">
            <sl-spinner
              style="font-size: 50px; --track-width: 10px;"
            ></sl-spinner>
          </div>`
        : this.renderTable()}
    `;
  }

  renderTable() {
    const { context, value: currentState } = this.searchMachine.getSnapshot();
    if (context.pkgs.length === 0)
      return html`<sl-alert open>
        <sl-icon slot="icon" name="info-circle"></sl-icon>
        No results
      </sl-alert> `;
    if (currentState === "error")
      return html`<sl-alert variant="danger" open>
        <sl-icon slot="icon" name="exclamation-circle"></sl-icon>
        Error getting packages, try again later
      </sl-alert> `;
    return html`
      <nav>
        <sl-button
          ?disabled=${!hasPrevCursor(context)}
          @click=${() => this.searchMachine.send({ type: "PREV" })}
        >
          <sl-icon name="caret-left-fill"></sl-icon>
        </sl-button>
        <sl-button
          ?disabled=${!hasNextCursor(context)}
          @click=${() => this.searchMachine.send({ type: "NEXT" })}
        >
          <sl-icon name="caret-right-fill"></sl-icon>
        </sl-button>
      </nav>
      <sl-menu>
        ${context.pkgs.map(
          ({ pname }) =>
            html`
              <sl-menu-item
                @click=${this.clickPackageHandler}
                value=${pname}
                class=${classMap({ selected: this.selectedPkg === pname })}
                >${pname}</sl-menu-item
              >
            `
        )}
      </sl-menu>
    `;
  }

  private async clickPackageHandler(ev: EventInput) {
    const name = ev.target.value.trim();
    this.selectedPkg = name;

    if (name) {
      let data: any;
      let error: boolean;
      try {
        data = await getGraph(name);
      } catch {
        error = true;
      }

      const options = {
        detail: { data, error },
        bubbles: true,
        composed: true,
      };
      this.dispatchEvent(
        new CustomEvent<QueryResultPayload>("query-result", options)
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
    .spinner-holder {
      margin-top: 3em;
      display: grid;
      place-items: center;
    }
    nav {
      display: flex;
      margin: 0.25rem 0;
    }
    sl-button:first-child {
      margin-right: auto;
    }
  `;
}

declare global {
  interface HTMLElementTagNameMap {
    "nix-search": NixSearch;
  }
}
