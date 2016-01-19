# bcproc

This simple library was developed for process broadcasting feature. It's kind or processes registry with ability to
broadcast messages to every process inside.

Usage example:

```erlang
 bcproc:start_server(broadcast_server_name),
 bcproc:add_client(broadcast_server_name, Pid),
 bcproc:broadcast(broadcast_server_name, Message).
```