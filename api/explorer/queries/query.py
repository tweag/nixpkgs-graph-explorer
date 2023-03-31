import enum
import uuid
from pprint import pprint
from typing import Optional

import networkx as nx
from gremlin_python.process.traversal import T
from gremlin_python.structure.graph import Path
from pydantic import BaseModel, Field

from explorer.queries.cytoscape import CytoscapeJs


class TableEntry(BaseModel):
    id: str
    neighbours: list[str]


class CytoData(BaseModel):
    table_data: list[TableEntry] = Field(alias="table-data")
    graph_data: CytoscapeJs = Field(alias="graph-data")


class WarningMessage(enum.Enum):
    CYTOSCAPE_CONVERSION_FAILED = "Could not generate Cytoscape data from graph"
    RESULT_IS_NOT_PLOTTABLE = (
        "Only Gremlin results consisting of paths can be plotted. "
        "Graph not generated."
    )
    NON_VERTEX_QUERY = (
        "Only vertex-based queries (g.V()...) can be plotted. Graph not generated."
    )


class QueryResult(BaseModel):
    raw: str
    warning: Optional[WarningMessage] = None
    cyto: Optional[CytoData] = None


class GremlinResult:
    """
    Class to encapsulate a Gremlin query and result.

    Attributes
    ----------
    client : gremlin_python.driver.client.Client
        Gremlin Client which manages its own connection pool
    query : str
        Gremlin query string
    clean_gremlin: bool
        If true, Python `T` Enums in Gremlin results will be replaced with sensible
        strings.

    Methods
    -------
    to_dict():
        create a result dictionary to be returned to the front-end as JSON
    """

    G = None
    cyto_data = None
    raw = ""
    result = []
    warning = None

    def __init__(self, client, query: str, clean_gremlin=False):
        self.client = client
        self.query = query.strip()
        self.clean_gremlin = clean_gremlin

        # the query is submitted to the Gremlin server
        self.__do_query()

        # the Gremlin query string possibly cleaned of Python Enums depending
        # on `clean_gremlin`
        self.__clean_raw_result()

        # a NetworkX graph object is possibly constructed from the
        self.__make_graph()

        # NetworkX graph is serialised to Cytoscape format
        self.__make_cyto_data()

    def __do_query(self):
        """
        Do Gremlin query
        """
        try:
            self.result = (
                self.client.submit(
                    self.query, request_options={"evaluationTimeout": 5000}
                )
                .all()
                .result()
            )
            self.raw = str(self.result)
        except Exception:
            raise ValueError("Could not get result from server or query is invalid")

    def __clean_raw_result(self):
        """
        Clean ugly T Enums in Gremlin result
        """
        if self.clean_gremlin:
            reprT = [
                (repr(T.id), "Id"),
                (repr(T.id_), "Id_"),
                (repr(T.key), "Key"),
                (repr(T.label), "Label"),
                (repr(T.value), "Value"),
            ]
            for k, v in reprT:
                self.raw = self.raw.replace(k, v)

    def to_query_result(self) -> QueryResult:
        """
        Create a result dictionary to be returned to the front-end as JSON
        """
        # Need to first parse cyto_data into expected type. This may fail.
        pprint(self.cyto_data)
        if self.cyto_data is not None:
            parsed_cyto_data = CytoData.parse_obj(self.cyto_data)
        else:
            parsed_cyto_data = None
        return QueryResult(raw=self.raw, warning=self.warning, cyto=parsed_cyto_data)

    def __make_graph(self):
        """
        Attempt to generate a NetworkX graph from the Gremlin result
        """

        # We only attempt to plot vertex-based queries, not edge-based
        if not self.query.startswith("g.V"):
            self.warning = WarningMessage.NON_VERTEX_QUERY
        G = nx.DiGraph()
        all_paths = True
        for p in self.result:
            match p:
                case Path():
                    pass
                case _:
                    all_paths = False
                    break
        if all_paths:
            for p in self.result:
                self.__add_path_to_graph(G, p)
            self.G = G
        else:
            self.warning = WarningMessage.RESULT_IS_NOT_PLOTTABLE

    @staticmethod
    def __add_path_to_graph(G: nx.Graph, path: Path) -> nx.Graph:
        """
        Add Gremlin Path to NetworkX graph
        """
        p = []
        for i in path.objects:
            if i:
                p.append(i)
            else:
                p.append(f"EMPTY-ID-{uuid.uuid4()}")

        nen = [p[i : i + 3] for i in range(len(p))[:-2:2]]
        for [node0, edge, node1] in nen:
            node0 = node0.replace(" ", "_")
            node1 = node1.replace(" ", "_")
            if node0 not in G:
                G.add_node(node0)
            if node1 not in G:
                G.add_node(node1)
            G.add_edge(node0, node1, label=edge)
        return G

    def __make_cyto_data(self):
        """
        Serialise NetworkX graph to Cytoscape data
        """
        try:
            if self.G:
                graph_data = nx.cytoscape_data(self.G)
                table_data = [
                    {"id": i, "neighbours": j}
                    for (i, j) in nx.to_dict_of_lists(self.G).items()
                ]
                self.cyto_data = {"graph-data": graph_data, "table-data": table_data}
        except Exception:
            self.warning = WarningMessage.CYTOSCAPE_CONVERSION_FAILED
