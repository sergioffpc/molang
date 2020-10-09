-module(molang_graphics).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([create_image/1, destroy_image/1]).
-export([create_object/1, destroy_object/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(LD_LIBRARY, ?MODULE).

-record(molang_graphics_state, {
  port :: port()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  case erl_ddll:load_driver(".", ?LD_LIBRARY) of
    ok -> ok;
    {error, already_loaded} -> ok;
    {error, Reason} -> exit({error, erl_ddll:format_error(Reason)})
  end,
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_image(Filename) ->
  gen_server:call(?SERVER, {create_image, Filename}).
destroy_image(Image) ->
  gen_server:cast(?SERVER, {destroy_image, Image}).

create_object(Image) ->
  gen_server:call(?SERVER, {create_object, Image}).
destroy_object(Object) ->
  gen_server:cast(?SERVER, {destroy_object, Object}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #molang_graphics_state{}} | {ok, State :: #molang_graphics_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  Port = erlang:open_port({spawn, ?LD_LIBRARY}, [binary]),
  {ok, #molang_graphics_state{port = Port}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #molang_graphics_state{}) ->
  {reply, Reply :: term(), NewState :: #molang_graphics_state{}} |
  {reply, Reply :: term(), NewState :: #molang_graphics_state{}, timeout() | hibernate} |
  {noreply, NewState :: #molang_graphics_state{}} |
  {noreply, NewState :: #molang_graphics_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #molang_graphics_state{}} |
  {stop, Reason :: term(), NewState :: #molang_graphics_state{}}).
handle_call({create_image, Filename}, _From, State = #molang_graphics_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#00), erlang:term_to_binary(Filename)]),
  {reply, {ok, wait_data(Port)}, State};
handle_call({create_object, Image}, _From, State = #molang_graphics_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#10), erlang:term_to_binary(Image)]),
  {reply, {ok, wait_data(Port)}, State}.


%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #molang_graphics_state{}) ->
  {noreply, NewState :: #molang_graphics_state{}} |
  {noreply, NewState :: #molang_graphics_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #molang_graphics_state{}}).
handle_cast({destroy_image, Image}, State = #molang_graphics_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#01), erlang:term_to_binary(Image)]),
  {noreply, State};
handle_cast({destroy_object, Object}, State = #molang_graphics_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#11), erlang:term_to_binary(Object)]),
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #molang_graphics_state{}) ->
  {noreply, NewState :: #molang_graphics_state{}} |
  {noreply, NewState :: #molang_graphics_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #molang_graphics_state{}}).
handle_info(_Info, State = #molang_graphics_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #molang_graphics_state{}) -> term()).
terminate(_Reason, _State = #molang_graphics_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #molang_graphics_state{},
    Extra :: term()) ->
  {ok, NewState :: #molang_graphics_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #molang_graphics_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(wait_data(Port :: port()) -> term()).
wait_data(Port) ->
  receive
    {Port, {data, Data}} ->
      erlang:binary_to_term(Data)
  end.
