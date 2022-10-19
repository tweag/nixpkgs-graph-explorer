from contextlib import closing
from pprint import pprint

from gremlin_python.driver.client import Client
# from gremlin_python.driver.request import RequestMessage
# from gremlin_python.driver.serializer import GraphSONMessageSerializer


def do_query(query):
    print("Instantiating client0")
    with closing(Client('ws://gremlin:8182/gremlin', 'g')) as client:
        # result_set = client.submit('g.V().limit(10).toList()')
        ro_query = query # + ".withStrategies(ReadOnlyStrategy)"
        pprint(ro_query)
        results = client.submit(ro_query, request_options={'evaluationTimeout': 5000}).all().result()
        pprint(results)
        client.close()
        return results

if __name__ == "__main__":
    do_query('g.V().elementMap().limit(10)')
