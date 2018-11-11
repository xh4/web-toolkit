-module(wt_handler).

-export([init/2]).

-include("wt_proto_http.hrl").

make_req(Req) ->
    #{host := LocalAddress,
      port := LocalPort,
      scheme := Scheme,
      method := Method,
      path := Path,
      qs := Query,
      version := Version,
      headers := Headers,
      peer := {RemoteAddress, RemotePort}
     } = Req,
    RemoteAddressStr = inet_parse:ntoa(RemoteAddress),
    Scheme1 = case Scheme of
                  <<"http">> -> 'HTTP';
                  <<"https">> -> 'HTTPS'
              end,
    Method1 = case Method of
                  <<"GET">> -> 'GET';
                  <<"HEAD">> -> 'HEAD';
                  <<"OPTIONS">> -> 'OPTIONS';
                  <<"PUT">> -> 'PUT';
                  <<"POST">> -> 'POST';
                  <<"DELETE">> -> 'DELETE'
              end,
    Version1 = case Version of
                   'HTTP/1.0' -> 'HTTP_1_0';
                   'HTTP/1.1' -> 'HTTP_1_1';
                   'HTTP/2' -> 'HTTP_2'
               end,
    Headers1 = maps:to_list(Headers),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Request = #'Request'{remote_address = RemoteAddressStr,
                         remote_port = RemotePort,
                         local_port = LocalPort,
                         scheme = Scheme1,
                         method = Method1,
                         path = Path,
                         query = Query,
                         version = Version1,
                         headers = Headers1,
                         body = Body},
    {Request, Req1}.

send_req(Socket, Req) ->
    {Request, Req1} = make_req(Req),
    Data = wt_proto_http:encode_msg(Request),
    Size = byte_size(Data),
    ok = gen_tcp:send(Socket, <<Size:32>>),
    ok = gen_tcp:send(Socket, Data),
    Req1.

make_rep(Response) ->
    Type = Response#'Response'.type,
    Code = Response#'Response'.code,
    Headers = Response#'Response'.headers,
    Body = Response#'Response'.body,
    Path = Response#'Response'.path,

    lager:info("~p", [Type]),
    lager:info("~p", [Code]),
    lager:info("~p", [Headers]),
    lager:info("~p", [Body]),
    lager:info("~p", [Path]),

    Headers1 = maps:from_list(Headers),

    {Code, Headers1, Body}.

recv_rep(Socket) ->
    case gen_tcp:recv(Socket, 4) of
        {ok, Data1} ->
            <<Size:32>> = Data1,
            case gen_tcp:recv(Socket, Size) of
                {ok, Data2} ->
                    Response = wt_proto_http:decode_msg(Data2, 'Response'),
                    make_rep(Response);
                {error, Why} ->
                    error(Why)
            end;
        {error, Why} ->
            error(Why)
    end.

init(Req, State) ->
    Host = "localhost",
    Port = 3003,
    {ok, Socket} = gen_tcp:connect(Host, Port,
                                   [binary, {packet, 0}, {active, false}]),
    Req1 = send_req(Socket, Req),
    {Code, Headers, Body} = recv_rep(Socket),
    ok = gen_tcp:close(Socket),
    Rep = cowboy_req:reply(Code, Headers, Body, Req1),
    {ok, Rep, State}.
