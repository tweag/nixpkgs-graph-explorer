from unittest.mock import MagicMock, patch

from websockets.exceptions import ConnectionClosedError


def test_unit_write_reconnects():
    """
    Check that writing to a connection which has been closed will trigger an
    expected number of reconnect attempts
    """
    from explorer.api.gremlin import WebSocketTransport

    MockClientConnection = MagicMock()
    MockClientConnection.return_value.send.side_effect = [
        ConnectionClosedError(None, None),
        ConnectionClosedError(None, None),
        None,
    ]
    mock_connect = MagicMock()
    mock_connect.return_value = MockClientConnection()

    with patch("websockets.sync.client.connect", new=mock_connect):
        transport = WebSocketTransport(max_retries=2)
        transport.connect("ws://mock_url")

        result = transport.write(b"foobar")

        assert result is None
        assert mock_connect.return_value.send.call_count == 3
        assert mock_connect.call_count == 3


def test_unit_read_reconnects():
    """
    Check that reading from a connection which has been closed will trigger an
    expected number of reconnect attempts
    """
    from explorer.api.gremlin import WebSocketTransport

    MockClientConnection = MagicMock()
    # Note: calling the mocked recv() will result in a ConnectionClosedError twice then
    # successfully return data.
    mock_data = b"foo"
    MockClientConnection.return_value.recv.side_effect = [
        ConnectionClosedError(None, None),
        ConnectionClosedError(None, None),
        b"foo",
    ]
    mock_connect = MagicMock()
    mock_connect.return_value = MockClientConnection()

    with patch("websockets.sync.client.connect", new=mock_connect):
        transport = WebSocketTransport(max_retries=2)
        transport.connect("ws://mock_url")

        result = transport.read()

        assert result == mock_data
        assert mock_connect.return_value.recv.call_count == 3
        assert mock_connect.call_count == 3
