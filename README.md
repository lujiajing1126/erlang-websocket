## Erlang WebSocket

### Websocket Protocal

HandShake:

```Erlang
<<"GET / HTTP/1.1\r\nHost: localhost:12345\r\nUser-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:22.0) Gecko/20100101 Firefox/22.0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: zh-cn,zh;q=0.8,en-us;q=0.5,en;q=0.3\r\nAccept-Encoding: gzip, deflate\r\nSec-WebSocket-Version: 13\r\nOrigin: null\r\nSec-WebSocket-Key: QbGS1EqgNlt+T16EbWS9Mg==\r\nConnection: keep-alive, Upgrade\r\nPragma: no-cache\r\nCache-Control: no-cache\r\nUpgrade: websocket">>
```