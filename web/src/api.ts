const API_URL = __API_URL__;

export interface Pkg {
  pname: string;
  outputPath: string;
}

export interface Cursor {
  direction: number;
  row_id: string;
}

export interface PackagesResponse {
  new_cursor: Cursor;
  packages: Pkg[];
}

export interface DataPayload {
  raw: string;
  cyto: {
    // There is https://www.npmjs.com/package/@types/cytoscape
    // but there is no types for "graph-data"
    "graph-data": any;
    "table-data": { id: string; neighbours: string[] };
  };
}
export interface QueryResultPayload {
  data?: DataPayload;
  error?: boolean;
}

interface IGetPackagesArgs {
  search: string;
  limit: number;
  cursor?: Cursor;
}

export async function getPackages({
  search = "",
  limit = 10,
  cursor,
}: IGetPackagesArgs): Promise<PackagesResponse> {
  const response = await fetch(`${API_URL}/packages`, {
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

export async function getGraph(pkgName: string) {
  return await rawQuery(
    `g.V().filter{it.get().value('pname').matches('${pkgName}')}.repeat(outE().otherV().simplePath()).times(2).path().by('pname').by(label)`
  );
}
