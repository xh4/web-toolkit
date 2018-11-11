%%%-------------------------------------------------------------------
%% @doc wt public API
%% @end
%%%-------------------------------------------------------------------

-module(wt_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
                                      {'_',
                                       [{"/static/[...]", cowboy_static, {dir, "/home/xh/rb/static/",
                                                                          [{mimetypes, cow_mimetypes, all}]}},
                                        {'_', wt_handler, []}]
                                      }
                                     ]),
    {ok, _} = cowboy:start_clear(wt_http_listener,
                                 [{port, 3002}],
                                 #{env => #{dispatch => Dispatch}}
                                ),
    wt_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================


%% spawn(fun () -> io:format('~p', [ rpc:call('lispnode@t630', foo, bar, []) ]) end).
