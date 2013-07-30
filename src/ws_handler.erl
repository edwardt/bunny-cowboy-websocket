-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {consumer=[]}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
        ConsumerPid = consumer:start_link(erlang_web, self()),
	{ok, Req, #state{consumer=ConsumerPid}}.

websocket_handle({text, Msg}, Req, State) ->
	{reply, {text, << "Got client post: ", Msg/binary >>}, Req, State};

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({rabbit, Msg}, Req, State) ->
    io:format("we got a rabbit message ~n"),
    {reply, {text, Msg}, Req,State};

websocket_info(Info, Req, State) ->
    io:format("We got a non rabbit message, odd~n"),
    io:format("Message is: ~p~n", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
