from contextlib import closing
import re
from gremlin_python.driver.client import Client
from gremlin_python.structure.graph import Path

import networkx as nx

class GremlinResult:
    prune_nix = False

    def __init__(self, data):
        self.raw = data
        self.make_graph()
        self.make_cyto_data()

    def to_dict(self):
        if self.cyto_data:
            return {"raw": str(self.raw), "cyto": self.cyto_data}
        else:
            return {"raw": str(self.raw)}

    def make_graph(self):
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
            self.G = None

    @staticmethod
    def add_path_to_graph(G:nx.Graph, path:Path, prune_nix=False)-> nx.Graph:
        p = path.objects
        nen = [p[i:i+3] for i in range(len(p))[:-2:2]]
        for [node0, edge, node1] in nen:
            if prune_nix:
                # node0 = re.sub("/nix/store/\w+-", "", node0)
                # node1 = re.sub("/nix/store/\w+-", "", node1)
                node0 = re.sub("/nix/store/", "", node0)
                node1 = re.sub("/nix/store/", "", node1)
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

def do_query(query:str)-> dict|None:
    results = []
    # Note: this hostname is aliased in docker-compose
    # with closing(Client('ws://gremlin:8182/gremlin', 'g')) as client:
    with closing(Client('ws://localhost:8182/gremlin', 'g')) as client:
        results = client.submit(query, request_options={'evaluationTimeout': 5000}).all().result()

    G = GremlinResult(results)
    return G.to_dict()


