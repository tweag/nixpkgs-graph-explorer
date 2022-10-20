from contextlib import closing
from pprint import pprint

from gremlin_python.driver.client import Client
# from gremlin_python.driver.request import RequestMessage
# from gremlin_python.driver.serializer import GraphSONMessageSerializer
from gremlin_python.structure.graph import Path

import networkx as nx

def do_query(query):
    print("Instantiating client0")
    # Note: this hostname is aliased in docker-compose
    results = []
    with closing(Client('ws://gremlin:8182/gremlin', 'g')) as client:
        # result_set = client.submit('g.V().limit(10).toList()')
        results = client.submit(query, request_options={'evaluationTimeout': 25000}).all().result()
    G = make_graph(results)
    graph_data = nx.cytoscape_data(G)
    table_data = [{'id': id_, 'name': i, 'neighbours': j} for (id_, (i, j))  in enumerate(nx.to_dict_of_lists(G).items())]
    data = {"graph-data": graph_data, "table-data": table_data}
    pprint(data)
    return data

def make_graph(paths:list[Path])-> nx.Graph:
    G = nx.Graph()
    for p in paths:
        add_path_to_graph(G, p)
    return G

def add_path_to_graph(G:nx.Graph, path:Path)-> nx.Graph:
    p = path.objects
    nen = [p[i:i+3] for i in range(len(p))[:-2:2]]
    for [node0, edge, node1] in nen:
        if not node0 in G:
            G.add_node(node0)
        if not node1 in G:
            G.add_node(node0)
        G.add_edge(node0, node1, label=edge)
    return G

