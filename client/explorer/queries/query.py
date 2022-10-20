from contextlib import closing
from pprint import pprint
import re
import json
from pymonad.maybe import Nothing, Just, Maybe

from gremlin_python.driver.client import Client
# from gremlin_python.driver.request import RequestMessage
# from gremlin_python.driver.serializer import GraphSONMessageSerializer
from gremlin_python.structure.graph import Path

import networkx as nx

def do_query(query:str)-> dict[str, dict]:
    results = []
    # Note: this hostname is aliased in docker-compose
    with closing(Client('ws://gremlin:8182/gremlin', 'g')) as client:
        results = client.submit(query, request_options={'evaluationTimeout': 5000}).all().result()
    data = (
        Maybe
        .insert(None)
        .then(lambda _: make_graph(results, prune_nix=True))
        .map(lambda g: {'g': g, 'd': nx.cytoscape_data(g)})
        .map(lambda a: {'graph-data': a['d'], 'table-data': [{'id': i, 'neighbours': j} for (i, j)  in nx.to_dict_of_lists(a['g']).items()]})
        ).maybe(None, lambda x: x)
    # G = make_graph(results, prune_nix=True).either(lambda x: 1, lambda x: x)
    # graph_data = nx.cytoscape_data(G)
    # table_data = [{'id': i, 'neighbours': j} for (i, j)  in nx.to_dict_of_lists(G).items()]
    # data = {"graph-data": graph_data, "table-data": table_data}
    return data

def make_graph(paths:list[Path], prune_nix: bool = False)-> Maybe[nx.Graph]:
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
        return Just(G)
    else:
        return Nothing

def add_path_to_graph(G:nx.Graph, path:Path, prune_nix=False)-> nx.Graph:
    p = path.objects
    nen = [p[i:i+3] for i in range(len(p))[:-2:2]]
    for [node0, edge, node1] in nen:
        if prune_nix:
            node0 = re.sub("/nix/store/\w+-", "", node0)
            node1 = re.sub("/nix/store/\w+-", "", node1)
        if not node0 in G:
            G.add_node(node0)
        if not node1 in G:
            G.add_node(node0)
        G.add_edge(node0, node1, label=edge)
    return G

