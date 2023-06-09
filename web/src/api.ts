const API_URL = __API_URL__;

export interface Derivation {
  output_path: string;
  attribute_path: string;
}

export interface Cursor {
  direction: number;
  row_id: string;
}

export interface DerivationsResponse {
  new_cursor: Cursor;
  derivations: Derivation[];
}

export interface DataPayload {
  raw: string;
  cyto: {
    // There is https://www.npmjs.com/derivation/@types/cytoscape
    // but there is no types for "graph-data"
    "graph-data": any;
    "table-data": { id: string; neighbours: string[] };
  };
}
export interface QueryResultPayload {
  data?: DataPayload;
  error?: boolean;
}

interface IGetDerivationsArgs {
  search: string;
  limit: number;
  cursor?: Cursor;
}

export async function getDerivations({
  search = "",
  limit = 10,
  cursor,
}: IGetDerivationsArgs): Promise<DerivationsResponse> {
  const response = await fetch(`${API_URL}/derivations`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      search_predicate: search,
      limit,
      cursor,
    }),
  });
  const queryResult = await response.json();
  return queryResult;
}

export async function rawQuery(query: string) {
  const response = await fetch(`${API_URL}/gremlin`, {
    method: "POST",
    headers: {
      Accept: "application/json",
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      query,
    }),
  });
  return await response.json();
}

export async function getGraph(outputPath: string) {
  return await rawQuery(
    `g.V().has('derivation','output_path','${outputPath}').repeat(outE().otherV().simplePath()).until(outE().count().is(0).or().loops().is(gte(2))).path().by('output_path').by('label')`
  );
}
