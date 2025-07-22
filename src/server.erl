-module(server).
-export([start/0, handle_connection/2]).

start() ->
  io:format("Starting Server ...\n"),
  ets:new(contact_registry, [named_table, public, set]),

  listen("127.0.0.1", 6969).

listen(IP, Port) ->
  io:format("Listening on ~p:~p\n", [IP, Port]),
  {ok, Socket} = gen_tcp:listen(Port, [
      binary,           % receive data as binaries
      {packet, 0},      % raw TCP stream (no packet framing)
      {reuseaddr, true},% allow reusing the address
      {active, false}   % passive mode (manual recv)
  ]),

  loop(Socket).

loop(Socket) ->
  {ok, Conn} = gen_tcp:accept(Socket),
  ID = helpers:random_id(),
  spawn(server, handle_connection, [Conn, ID]),
  loop(Socket). 

handle_connection(Conn, ID) ->
  case gen_tcp:recv(Conn, 0) of
    {ok, Recv} ->
      Stringified = string:trim(binary_to_list(Recv)),
      case Stringified of
        "ping" ->
          gen_tcp:send(Conn, <<"pong\n\n">>),
          handle_connection(Conn, ID);
        "pong" ->
          gen_tcp:send(Conn, <<"ping\n\n">>),
          handle_connection(Conn, ID);
        "list" ->
          Entries = ets:tab2list(contact_registry),
          Keys    = [Keys || {Keys, _Value} <- Entries, Keys /= ID],
          Msg = io_lib:format("List: ~p\n\n", [Keys]),
          gen_tcp:send(Conn, list_to_binary(Msg)),
          handle_connection(Conn, ID);
        "register" ->
            {ok, {IP, Port}} = inet:peername(Conn),
            ets:insert(contact_registry, {ID, {Conn, IP, Port}}),
            gen_tcp:send(Conn, <<"Registered\n\n">>),
            handle_connection(Conn, ID);
        "help" ->
          gen_tcp:send(Conn, 
            <<
              "Commands:      \n"
              "   -- ping     \n"
              "   -- pong     \n"
              "   -- dial     \n"
              "   -- register \n"
              "   -- list     \n"
              "\n\n"
            >>

          ),
          handle_connection(Conn, ID);
        _ ->
          case string:str(Stringified, "dial") of
            Pos when Pos > 0 ->
              try
                Contact = string:substr(Stringified, Pos+1+length("dial")),
                dial(Conn, Contact),
                handle_connection(Conn, ID)
              catch
                Class:Reason ->
                  io:format("Error caught: ~p: ~p~n", [Class, Reason]),
                  handle_connection(Conn, ID)
              end;
            _ -> 
              ok
          end
      end,
      gen_tcp:send(Conn, <<"Command Not Found\n">>),
      handle_connection(Conn, ID);
  {error, _} ->
      Deleted = ets:delete(contact_registry, ID),
      io:format("Deleted ~p ~p\n", [ID, Deleted]),
      ok
  end.

dial(Conn, Contact) ->
    ContactReg = list_to_integer(Contact),
    Result = ets:lookup(contact_registry, ContactReg),

    case Result of
      [] ->
        gen_tcp:send(Conn, <<"Contact not Found..\n">>);
      [{_, Value}] ->
        {OtherConn, _, _} = Value,
        
        % This is supposed to come from the dialer not the server

        Cmd = "ffmpeg -f avfoundation -i \":0\" -c:a libopus -f ogg -",
        Port = open_port({spawn, Cmd}, [exit_status]),

        wait_for_exit(Port, OtherConn)

      end,

    gen_tcp:send(Conn, <<"\n">>),
    ok.

wait_for_exit(Port, Socket) ->
    receive
        {_, {exit_status, Status}} ->
            io:format("ffmpeg exited with status: ~p~n", [Status]);
        {_, {data, Data}} ->
            io:format("Received data: ~p~n", [length(Data)]),
            gen_tcp:send(Socket, Data),
            wait_for_exit(Port, Socket)
    end.

