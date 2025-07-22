-module(main).

-export([start/0]).

start() ->
  case client_connect("127.0.0.1", 6969) of
    {ok, Socket} ->
      open_mic(Socket),
      ok;
    {error, Reason} ->
      io:format("~p\n", [Reason])
   end.

client_connect(IP, Port) ->
  gen_tcp:connect(IP, Port, [binary, {active, false}]).

open_mic(Socket) ->
  Cmd = "ffmpeg -f avfoundation -i \":0\" -c:a libopus -f ogg -",
  Port = open_port({spawn, Cmd}, [exit_status]),
  io:format("Listening ...\n"),
  loop(Socket, Port),
  ok.

loop(Socket, Port)->
  receive
    {_, {data, Data} } ->
      send_data_to_socket(Socket, Data),
      loop(Socket, Port);
    {_, {exit_status, Status}} ->
      io:format("Getting Status ~p\n", [Status]),
      ok;
    {_, closed} ->
      ok
  end.

send_data_to_socket(Socket, Data) ->
  gen_tcp:send(Socket, Data).

