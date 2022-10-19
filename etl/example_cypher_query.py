from contextlib import closing
from pprint import pprint

from gremlin_python.driver.client import Client
from gremlin_python.driver.request import RequestMessage
from gremlin_python.driver.serializer import GraphSONMessageSerializer

from time import sleep

def main():

    # print("Instantiating client0")
    # client = Client('ws://localhost:8182/gremlin', 'g')
    # # # result_set = client.submit('g.V().limit(10).toList()')
    # # result_set = client.submit("g.withStrategies(ReadOnlyStrategy).V().count()")
    # results = client.submit("g.V().as('n').project('n').by(__.identity()).limit(10).project('n').by(__.select('n').valueMap().with('~tinkerpop.valueMap.tokens'))").all().result()
    # pprint(results)
    # client.close()

    # # sleep(1)

    # #----------------------------------------------------------------------------------------------------
    # # Cypher
    # #----------------------------------------------------------------------------------------------------
    serializer = GraphSONMessageSerializer()
    # workaround to avoid exception on any opProcessor other than `standard` or `traversal`:
    serializer.cypher = serializer.standard
    print("Instantiating client1")
    client = Client('ws://localhost:8182/gremlin', 'g', message_serializer=serializer)
    cypherQuery = 'MATCH (n) RETURN n LIMIT 10'
    message = RequestMessage('cypher', 'eval', {'gremlin': cypherQuery, 'evaluationTimeout': 5000})
    print("Submitting query to gremlin server")
    results = client.submit(message).all().result()
    pprint(results)
    client.close()

if __name__ == "__main__":
    main()
