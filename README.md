# bcproc

This simple library was developed for process broadcasting feature. It's kind or processes registry with ability to
broadcast messages to each process inside.

Usage example:

```erlang

% start a pool of processes to broadcast to
 bcproc:start_server(broadcast_server_name),
 
% add Pid of process to previously created pool
 bcproc:add_client(broadcast_server_name, Pid),
 
% Broacast a Message to processes added to the pool
 bcproc:broadcast(broadcast_server_name, Message).

```

Each process will get a message like *{bcproc, Message}* which can be handled with *handle_info* functions of gen_* behaviours
