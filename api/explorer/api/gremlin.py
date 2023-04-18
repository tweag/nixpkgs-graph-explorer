"""Utilities for working with Gremlin Server"""


from ssl import SSLContext
from typing import Any, Callable

import backoff
from gremlin_python.driver import serializer
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.driver.transport import AbstractBaseTransport
from websockets.exceptions import ConnectionClosedError
from websockets.sync import client
from websockets.typing import LoggerLike


class GremlinTransportError(Exception):
    """Indicates an error in the Gremlin transport"""


def default_remote_connection(
    url: str, traversal_source: str = "g"
) -> DriverRemoteConnection:
    """Creates a Gremlin remote connection using the `nixpkgs-graph-explorer` defaults

    Args:
        url (str): The url of the Gremlin Server to connect to
        traversal_source (str, optional): The name of the graph traversal source to
            use. Note that this is expected to be pre-configured on the target
            Gremlin Server. Defaults to "g".

    Returns:
        DriverRemoteConnection: the remote connection
    """
    return DriverRemoteConnection(
        url,
        traversal_source,
        message_serializer=serializer.GraphSONMessageSerializer(),
        transport_factory=WebSocketTransport,
    )


class WebSocketTransport(AbstractBaseTransport):
    """
    An alternative to `gremlinpython`'s `AiohttpTransport` with more
    robust error handling. Built using `websocket`.
    """

    def __init__(
        self,
        max_retries: int = 3,
        read_timeout=30,
        ssl_context: SSLContext | None = None,
        open_timeout: float | None = 10,
        close_timeout: float | None = 10,
        logger: LoggerLike | None = None,
    ) -> None:
        """Initializes the transport.

        No connections are opened until connect() is called.

        Args:
            max_retries (int, optional): The maximum number of times to reconnect and
                retry IO operations in case of connection errors. Defaults to 3.
            read_timeout (int, optional): Timeout for read operations, in seconds.
                Defaults to 30.
            ssl_context (SSLContext | None, optional): An optional SSL context to
                provide to the websocket client. Defaults to None.
            open_timeout (float | None, optional): Timeout for opening client
                connections, in seconds. Defaults to 10.
            close_timeout (float | None, optional): Timeout for closing client
                connections, in seconds. Defaults to 10.
            logger (LoggerLike | None, optional): Optional logger to pass to the
                underlying websocket client. If unspecified the default one specified
                by the websocket library will be used. Defaults to None.
        """
        self.max_retries = max_retries
        self.ssl_context = ssl_context
        self.open_timeout = open_timeout
        self.close_timeout = close_timeout
        self.connection_logger = logger
        self.read_timeout = read_timeout
        self._client: client.ClientConnection | None = None
        self._last_connect_url: str | None = None
        self._last_connect_headers: dict | None = None

    def unsafe_get_client(self) -> client.ClientConnection:
        """Gets the transport's current websocket client connection

        Raises:
            GremlinTransportError: If a client has not yet been created using connect()

        Returns:
            ClientConnection: the websocket client connection
        """
        if self._client is None:
            raise GremlinTransportError(
                "WebSocketTransport.unsafe_get_client() was called, but no client has "
                "configured. Make sure that connect() is called before this method."
            )
        return self._client

    @backoff.on_exception(backoff.expo, OSError, max_tries=3)
    def connect(self, url: str, headers: dict | None = None) -> None:
        """Initializes the websocket client if one does not already exist.

        Args:
            url (str): The url of the websocket server.
            headers (dict | None, optional): Extra HTTP headers to include in
                requests. Defaults to None.
        """
        if self._client is not None:
            self.close()

        self._client = client.connect(
            uri=url,
            additional_headers=headers,
            open_timeout=self.open_timeout,
            close_timeout=self.close_timeout,
            ssl_context=self.ssl_context,
            logger=self.connection_logger,
        )
        self._last_connect_url = url
        self._last_connect_headers = headers

    @property
    def with_reconnect(self) -> Callable[[Callable[..., Any]], Callable[..., Any]]:
        """
        A decorator which will retry its input function with exponential back-off
        if unexpected websocket connection errors are raised.

        Raises:
            GremlinTransportError: If connect() has not already been called at
                least once.
        """
        if self._last_connect_url is None:
            raise GremlinTransportError(
                "Attempted to define a with_reconnect instance, however no "
                "previously used url was found. This should not be possible. Ensure "
                "that connect() has been called at least once before calling this "
                " method."
            )
        url = self._last_connect_url
        headers = self._last_connect_headers
        return backoff.on_exception(
            backoff.expo,
            ConnectionClosedError,
            max_tries=self.max_retries + 1,
            on_backoff=(lambda _: self.connect(url, headers)),
        )

    def write(self, message: bytes) -> None:
        """Writes data to the current client connection

        Args:
            message (bytes): The data to write
        """
        # Note: we apply the decorator in an anonymous function here so that we can
        # reference `self` for attempting reconnects
        self.with_reconnect(lambda: self.unsafe_get_client().send(message))()

    def read(self) -> bytes:
        """Reads the next message from the current client connection

        Returns:
            bytes: The message data. If the server returned text data it will be
                encoded as a UTF-8 string.
        """
        data = self.with_reconnect(
            lambda: self.unsafe_get_client().recv(timeout=self.read_timeout)
        )()
        # Note: Mimicking gremlinpython's approach to homogenizing the output type here
        if isinstance(data, str):
            data = data.strip().encode("utf-8")
        return data

    def close(self) -> None:
        """Closes the websocket client, if one exists."""
        if self._client is not None:
            self._client.close()
            self._client = None

    @property
    def closed(self) -> bool:
        """Flag indicating whether the transport's websocket client has been closed"""
        return self._client is None
