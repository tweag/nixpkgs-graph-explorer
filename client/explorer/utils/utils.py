import time
import requests
import json


class Graph:

    def __init__(self, value, branches):
        self.value = value
        self.branches = branches


data = {

        }


def do_things(_):
    time.sleep(1)
    elements = [
        {"data": { "id": "a" }},
        {"data": { "id": "b" }},
        {"data": { "id": "ab", "source": "a", "target": "b" }}
    ]
    return elements

def load_json(st):
    if 'http' in st:
        return requests.get(st).json()
    else:
        with open(st, 'rb') as f:
            x = json.load(f)
        return x


