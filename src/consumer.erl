-module(consumer).

-include_lib("eunit/include/eunit.hrl").

-behavior(gen_bunny).

-export([start_link/1,
         stop/1]).

-export([init/1, init/2,
         handle_message/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-export([ack_stuff/2, get_messages/1]).

-include("deps/gen_bunny/include/gen_bunny.hrl").

-record(state, {messages=[], calls=[], infos=[], casts=[], wspid=[]}).


start_link(WebSocketPid) ->
    [A,B] = tl(string:tokens(erlang:pid_to_list(self()),"<>.")),
    NameAsList = "consumer-" ++ A ++ "-" ++ B, 
    NameAsAtom = list_to_atom(NameAsList),
    NameAsBin = list_to_binary(NameAsList),
    Exchange =  #'exchange.declare'{exchange = <<"fanout">>, type= <<"fanout">>, durable=true},
    Queue = #'queue.declare'{queue = NameAsBin},
    RoutingKey = <<"">>,
    ConnectInfo = {network, "localhost", 5672, {<<"guest">>, <<"guest">>}, <<"/">>},
    DeclareInfo = {Exchange,Queue, RoutingKey},
    gen_bunny:start_link({local, NameAsAtom}, ?MODULE,  ConnectInfo, DeclareInfo, [WebSocketPid]).

init([Args]) ->
    io:format("~p: init", [?MODULE]),
    io:format("Args: ~p~n", [Args]),
    {ok, #state{wspid=Args}}.

init(_One,_Two) ->
    io:format("INIT TWO~n"),
    {ok, #state{}}.

ack_stuff(Pid, Tag) ->
    gen_bunny:cast(Pid, {ack_stuff, Tag}).

get_messages(Pid) ->
    gen_bunny:call(Pid, get_messages).
    
stop(Pid) ->
    gen_bunny:call(Pid, stop).

handle_message(Message, State=#state{messages=Messages,wspid=WsPid})  when ?is_message(Message) orelse ?is_tagged_message(Message) ->
    {amqp_msg, _Rest , Payload} = Message,
    io:format("| ~p going to send payload: ~p to ~p~n", [?MODULE, Payload, WsPid]),
    WsPid ! {rabbit,Payload},
    NewMessages = [Message|Messages],
    {noreply, State#state{messages=NewMessages}};

handle_message(Message, State) ->
    io:format("Got rubbish: ~p~n", [Message]),
    {norely, State}.

handle_call(get_messages, _From, _State=#state{messages=Messages}) ->
    io:format("Getting messages~n", []),
    {reply, Messages, #state{messages=[]}}.

handle_cast(Msg, State=#state{casts=Casts}) ->
    {noreply, State#state{casts=[Msg|Casts]}}.

handle_info(Info, State=#state{infos=Infos}) ->
    io:format("INFO~n"),
    {noreply, State#state{infos=[Info|Infos]}}.

terminate(Reason, _State) ->
    io:format("~p terminating with reason ~p~n", [?MODULE, Reason]),
    ok.


