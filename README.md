## Erlang WebSocket

### Install

```
erl -make
```

### Startup

```
cd dest
erl
1> userservice:start().
true
2> websocket:start().
listen on 12345
```

### Websocket Protocol

HandShake:

Step One:

```Erlang
<<"GET / HTTP/1.1\r\n
Host: localhost:12345\r\n
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:22.0) Gecko/20100101 Firefox/22.0\r\n
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n
Accept-Language: zh-cn,zh;q=0.8,en-us;q=0.5,en;q=0.3\r\n
Accept-Encoding: gzip, deflate\r\n
Sec-WebSocket-Version: 13\r\nOrigin: null\r\n
Sec-WebSocket-Key: QbGS1EqgNlt+T16EbWS9Mg==\r\n
Connection: keep-alive, Upgrade\r\n
Pragma: no-cache\r\n
Cache-Control: no-cache\r\n
Upgrade: websocket">>
```

Step Two:

```Erlang
<<"HTTP/1.1 101 Switching Protocols\r\n">>,
<<"Upgrade: websocket\r\n">>,
<<"Connection: Upgrade\r\n">>,
<<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
<<"\r\n">>
```

Which Base64 is the result of ```Sha1 = crypto:hash(sha,[SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>])``` in BASE64