import time
import requests
import json
import networkx as nx


def do_things(_):
    time.sleep(1)
    example = nx.lollipop_graph(10, 20)
    graph_data = nx.cytoscape_data(example)
    table_data = [{'id': id_, 'name': i, 'neighbours': j} for (id_, (i, j))  in enumerate(nx.to_dict_of_lists(example).items())]
    return {"graph-data": graph_data, "table-data": table_data}

def load_json(st):
    if 'http' in st:
        return requests.get(st).json()
    else:
        with open(st, 'rb') as f:
            x = json.load(f)
        return x


