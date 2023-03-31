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

export async function getPackages({
  search = "",
  limit = 10,
  cursor = null,
}): Promise<PackagesResponse> {
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

export async function getGraph(pkgName: string) {
  const response = await fetch(`${API_URL}/gremlin`, {
    method: "POST",
    headers: {
      Accept: "application/json",
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      query: `g.V().filter{it.get().value('pname').matches('${pkgName}')}.repeat(outE().otherV().simplePath()).times(2).path().by('pname').by(label)`,
    }),
  });
  return await response.json();
}
