-module(client).
-export([start/0, receive_center/2, command_center/1]).

start()->
  {ok, Socket} = gen_tcp:connect("127.0.0.1", 6969, [binary, {packet, 0}, {active, false}]),
  PapaPid = self(),
  spawn(client, receive_center, [Socket, PapaPid]),
  spawn(client, command_center, [Socket]),
  loop(Socket),
  ok.

loop(Socket) ->
  receive
    {recv_msg, OutputFormatted} ->
      io:format("~s\n", [OutputFormatted]),
      command_center(Socket)
  end,
  loop(Socket).

receive_center(Socket, PapaPid) ->
  {ok, Output} = gen_tcp:recv(Socket, 0),
  OutputFormatted = binary_to_list(Output),
  PapaPid ! {recv_msg, OutputFormatted},
  receive_center(Socket, PapaPid).

command_center(Socket) ->
  Input = io:get_line("> "),
  TrimmedInput = string:trim(Input),
  case TrimmedInput of 
    "help" ->
      gen_tcp:send(Socket, <<"help">>);
    "list" ->
      gen_tcp:send(Socket, <<"list">>);
    "register" ->
      gen_tcp:send(Socket, <<"register">>);
    "ping" ->
      gen_tcp:send(Socket, <<"ping">>);
    "pong" ->
      gen_tcp:send(Socket, <<"pong">>);
      _ ->
        case string:prefix(TrimmedInput, "dial") of
          Rest when Rest =/= nomatch ->
            io:format("Dialing .. ~s\n", [Rest]),
            Command = "dial" ++ Rest,
            gen_tcp:send(Socket, list_to_binary(Command));
          nomatch ->
            io:format("Input does not start with dial\n")
        end
  end.

