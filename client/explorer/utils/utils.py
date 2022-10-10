import time
import requests
import json


def do_things(query):
    time.sleep(3)
    return query

def load_json(st):
    if 'http' in st:
        return requests.get(st).json()
    else:
        with open(st, 'rb') as f:
            x = json.load(f)
        return x


