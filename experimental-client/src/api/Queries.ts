export interface PackageInfo {
  /** The name of the package */
  name: string;
}

export interface GremlinResult {
  /** The raw Gremlin query output */
  raw: string;
  /** Optional warning message returned by the API */
  warning: string | null;
  /** Cytoscape graph data in cyjs format.
   *
   * See the docs here for more details: https://manual.cytoscape.org/en/stable/Supported_Network_File_Formats.html?highlight=json#cytoscape-js-json
   */
  cyto: any; // FIXME: Can we refine this into a more precise type?
}

export interface QueryError {
  httpResponseCode: number;
  message: string;
}

/** Attempts to read the API server's address from the `NIXPKGS_API_HOST` environment variable.
 * If this is not set http://localhost:3000 will be used as a default.
 * */
const getHostUrl = () =>
  process.env.NIXPKGS_API_HOST || "http://localhost:3000";

/** Fetches package names from the API */
export async function fetchPackageNames(): Promise<PackageInfo[] | QueryError> {
  await new Promise((r) => setTimeout(r, 5000));
  const rawResponse = await fetch(`${getHostUrl()}/packages`, {
    method: "GET",
  });

  if (rawResponse.status >= 400) {
    const m = await rawResponse.text();
    return {
      httpResponseCode: rawResponse.status,
      message: m,
    };
  }

  // We expect the response body to be in JSON format
  const responseBody = await rawResponse.json();

  // FIXME: Clean up this parsing logic a bit
  if ("packages" in responseBody) {
    return responseBody.packages;
  } else {
    return [];
  }
}

/** Submits a Gremlin query to the API and returns its results */
export async function submitGremlinQuery(
  query: string
): Promise<GremlinResult | QueryError> {
  await new Promise((r) => setTimeout(r, 5000));
  const rawResponse = await fetch(`${getHostUrl()}/gremlin`, {
    method: "POST",
    headers: {
      Accept: "application/json",
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ query: query }),
  });

  if (rawResponse.status >= 400) {
    const m = await rawResponse.text();
    return {
      httpResponseCode: rawResponse.status,
      message: m,
    };
  }

  // We expect the response body to be in JSON format
  const responseBody = await rawResponse.json();

  // FIXME: Add validation logic for response here
  return responseBody;
}
