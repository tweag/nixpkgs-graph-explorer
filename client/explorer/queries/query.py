from contextlib import closing
import re
from gremlin_python.driver.client import Client
from gremlin_python.structure.graph import Path

import networkx as nx

def do_query(query:str)-> dict[str, dict]|None:
    results = []
    # Note: this hostname is aliased in docker-compose
    # with closing(Client('ws://gremlin:8182/gremlin', 'g')) as client:
    with closing(Client('ws://localhost:8182/gremlin', 'g')) as client:
        results = client.submit(query, request_options={'evaluationTimeout': 5000}).all().result()

    G = make_graph(results, prune_nix=True)
    match G:
        case None:
            return None
        case _:
            return make_cyto_data(G)

def make_cyto_data(G:nx.Graph)-> dict[str, dict]:
    graph_data = nx.cytoscape_data(G)
    table_data = [{'id': i, 'neighbours': j} for (i, j)  in nx.to_dict_of_lists(G).items()]
    data = {"graph-data": graph_data, "table-data": table_data}
    return data

def make_graph(paths:list[Path], prune_nix: bool = False)-> nx.Graph|None:
    G = nx.Graph()
    all_paths = True
    for p in paths:
        match p:
            case Path():
                pass
            case _:
                all_paths = False
                break
    if all_paths:
        for p in paths:
            add_path_to_graph(G, p, prune_nix)
        return G
    else:
        return None

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

