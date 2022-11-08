from contextlib import closing
from gremlin_python.driver.client import Client
from gremlin_python.structure.graph import Path
from gremlin_python.process.traversal import T

import uuid

import networkx as nx


class GremlinResult:
    # Note: this hostname is aliased in docker-compose
    gremlin_host = 'ws://gremlin:8182/gremlin'
    prune_nix = False
    clean_gremlin = False
    warning = None
    cyto_data = None
    G = None
    result = []
    raw = ""

    def __init__(self, query:str, clean_gremlin=False):
        self.query = query.strip()
        self.clean_gremlin = clean_gremlin
        self.do_query()
        self.clean_raw_result()
        self.make_graph()
        self.make_cyto_data()

    def do_query(self):
        """
        Do Gremlin query
        """
        try:
            with closing(Client(self.gremlin_host, 'gReadOnly')) as client:
                self.result = client.submit(self.query, request_options={'evaluationTimeout': 5000}).all().result()
                self.raw = str(self.result)
        except Exception:
            raise ValueError("Could not get result from server or query is invalid")

    def clean_raw_result(self):
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
            for (k, v) in  reprT:
                self.raw = self.raw.replace(k, v)

    def to_dict(self):
        """
        Create a result dictionary to be returned to the front-end as JSON
        """
        r = {"raw":  self.raw}
        if self.warning:
            r["warning"] = self.warning
        if self.cyto_data:
            r["cyto"] = self.cyto_data
        return r

    def make_graph(self):
        """
        Generate a NetworkX graph from the Gremlin result
        """
        try:
            # We only attempt to plot vertex-based queries, not edge-based
            if not self.query.startswith('g.V'):
                raise ValueError("Only vertex-based queries (g.V()...) can be plotted. Graph not generated.")
            G = nx.Graph()
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
                    self.add_path_to_graph(G, p)
                self.G = G
            else:
                raise ValueError("Only Gremlin results consisting of paths can be plotted. Graph not generated.")
        except Exception as e:
            self.warning = str(e)

    @staticmethod
    def add_path_to_graph(G:nx.Graph, path:Path)-> nx.Graph:
        """
        Add Gremlin Path to NetworkX graph
        """
        p = []
        for i in path.objects:
            if i:
                p.append(i)
            else:
                p.append(f'EMPTY-ID-{uuid.uuid4()}')

        nen = [p[i:i+3] for i in range(len(p))[:-2:2]]
        for [node0, edge, node1] in nen:
            node0 = node0.replace(' ', '_')
            node1 = node1.replace(' ', '_')
            if not node0 in G:
                G.add_node(node0)
            if not node1 in G:
                G.add_node(node1)
            G.add_edge(node0, node1, label=edge)
        return G

    def make_cyto_data(self):
        """
        Convert NetworkX graph to Cytoscape data
        """
        try:
            if self.G:
                    graph_data = nx.cytoscape_data(self.G)
                    table_data = [{'id': i, 'neighbours': j} for (i, j)  in nx.to_dict_of_lists(self.G).items()]
                    self.cyto_data = {"graph-data": graph_data, "table-data": table_data}
        except Exception:
            self.warning = "Could not generate Cytoscape data from graph"

