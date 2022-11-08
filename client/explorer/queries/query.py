from contextlib import closing
from gremlin_python.driver.client import Client
from gremlin_python.structure.graph import Path
from gremlin_python.process.traversal import T

import networkx as nx

class GremlinResult:
    # Note: this hostname is aliased in docker-compose
    gremlin_host = 'ws://gremlin:8182/gremlin'
    prune_nix = False
    clean_gremlin = False
    warning = None
    cyto_data = None
    G = None

    def __init__(self, query:str, clean_gremlin=False):
        self.query = query.strip()
        self.clean_gremlin = clean_gremlin
        self.do_query()
        self.make_graph()
        self.make_cyto_data()

    def do_query(self):
        with closing(Client(self.gremlin_host, 'gReadOnly')) as client:
            self.raw = client.submit(self.query, request_options={'evaluationTimeout': 5000}).all().result()

    @staticmethod
    def cleanTs(raw):
        reprT = [
            (repr(T.id), "Id"),
            (repr(T.id_), "Id_"),
            (repr(T.key), "Key"),
            (repr(T.label), "Label"),
            (repr(T.value), "Value"),
        ]
        for (k, v) in  reprT:
            raw = raw.replace(k, v)
        return raw

    def to_dict(self):
        r = {"raw":  self.cleanTs(str(self.raw)) if self.clean_gremlin else str(self.raw)}
        if self.warning:
            r["warning"] = self.warning
        if self.cyto_data:
            r["cyto"] = self.cyto_data
        return r

    def make_graph(self):
        print(0, 'make_graph')
        try:
            # We only attempt to plot vertex-based queries, not edge-based
            if not self.query.startswith('g.V'):
                raise ValueError("Only vertex-based queries (g.V()...) will be plotted.")
            G = nx.Graph()
            all_paths = True
            for p in self.raw:
                match p:
                    case Path():
                        pass
                    case _:
                        all_paths = False
                        break
            if all_paths:
                for p in self.raw:
                    self.add_path_to_graph(G, p, self.prune_nix)
                self.G = G
            else:
                raise ValueError("Only Gremlin results consisting of paths will be plotted.")
        except Exception as e:
            self.warning = str(e)
            self.G = None

    @staticmethod
    def add_path_to_graph(G:nx.Graph, path:Path, prune_nix=False)-> nx.Graph:
        p = path.objects
        nen = [p[i:i+3] for i in range(len(p))[:-2:2]]
        for [node0, edge, node1] in nen:
            if prune_nix:
                node0 = node0.replace("/nix/store/", "")
                node1 = node1.replace("/nix/store/", "")
            if not node0 in G:
                G.add_node(node0)
            if not node1 in G:
                G.add_node(node0)
            G.add_edge(node0, node1, label=edge)
        return G

    def make_cyto_data(self):
        if self.G:
            graph_data = nx.cytoscape_data(self.G)
            table_data = [{'id': i, 'neighbours': j} for (i, j)  in nx.to_dict_of_lists(self.G).items()]
            self.cyto_data = {"graph-data": graph_data, "table-data": table_data}
        else:
            self.cyto_data = None

