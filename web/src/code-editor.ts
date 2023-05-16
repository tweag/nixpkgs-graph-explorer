import { LitElement, html, css } from "lit";
import { customElement, queryAsync, state } from "lit/decorators.js";

import { closeBrackets } from "@codemirror/autocomplete";
import { history, emacsStyleKeymap } from "@codemirror/commands";
import { bracketMatching, indentOnInput } from "@codemirror/language";
import { EditorState } from "@codemirror/state";
import { EditorView, keymap, highlightActiveLine } from "@codemirror/view";

import { QueryResultPayload, rawQuery } from "./api";

@customElement("code-editor")
export class CodeEditor extends LitElement {
  static styles = css`
    :host {
      display: block;
    }
    #editor {
      height: 100%;
    }
    .cm-editor {
      border: 1px solid var(--sl-panel-border-color);
      max-height: 100%;
    }
    sl-button {
      width: 100%;
      margin-bottom: 1rem;
    }
  `;

  @queryAsync("#editor") _editor: Promise<HTMLElement>;
  @state() _waiting = false;
  #editor?: EditorView;

  connectedCallback() {
    super.connectedCallback();
    this.addEditor();
  }
  async addEditor() {
    const editor = await this._editor;
    let startState = EditorState.create({
      doc: `g.V()
 .has(
   'package',
    'pname',
    'python3'
    // Package name goes here
 )
 .repeat(
   outE()
  .otherV()
  .simplePath())
 .until(
   outE()
  .count()
  .is(0)
  .or()
  .loops()
  .is(gte(2))
  // The value in gte() limits
  // the depth of the traversal
 )
 .path()
 .by('pname')
 .by('label')
 .limit(20)
  // This limits the number of "paths" returned
  // (where a path = [vertex, edge, vertex])`,

      extensions: [
        history(),
        closeBrackets(),
        bracketMatching(),
        highlightActiveLine(),
        keymap.of(emacsStyleKeymap),
        indentOnInput(),
      ],
    });

    this.#editor = new EditorView({
      state: startState,
      parent: editor,
      root: this.shadowRoot,
    });
  }
  async runQuery() {
    this._waiting = true;
    const query = this.#editor.state.doc.toString();

    let data: any;
    let error: boolean;

    try {
      data = await rawQuery(query);
    } catch {
      error = true;
    } finally {
      this._waiting = false;
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

  render() {
    return html`
      <sl-button @click=${this.runQuery} ?disabled=${this._waiting}>
        Execute query
      </sl-button>
      <div id="editor"></div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "code-editor": CodeEditor;
  }
}
