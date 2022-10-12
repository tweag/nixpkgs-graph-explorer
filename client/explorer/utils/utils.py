import time
import requests
import json
import networkx as nx


def do_things(_):
    # time.sleep(1)
    constructor = [(10, 20, 0.8), (20, 40, 0.8)]
    G = nx.random_shell_graph(constructor)
    # G = nx.lollipop_graph(10, 20)
    graph_data = nx.cytoscape_data(G)
    table_data = [{'id': id_, 'name': i, 'neighbours': j} for (id_, (i, j))  in enumerate(nx.to_dict_of_lists(G).items())]
    return {"graph-data": graph_data, "table-data": table_data}

def load_json(st):
    if 'http' in st:
        return requests.get(st).json()
    else:
        with open(st, 'rb') as f:
            x = json.load(f)
        return x


