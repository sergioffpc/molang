-module(molang_audio).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([create_buffer/1, destroy_buffer/1]).
-export([create_emitter/1, destroy_emitter/1]).

-export([play/1, stop/1]).

-export([set_emitter_position/3, set_emitter_velocity/3, set_emitter_direction/3]).
-export([set_listener_position/2, set_listener_velocity/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(LD_LIBRARY, ?MODULE).

-record(molang_audio_state, {
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

create_buffer(Filename) ->
  gen_server:call(?SERVER, {create_buffer, Filename}).
destroy_buffer(Buffer) ->
  gen_server:cast(?SERVER, {destroy_buffer, Buffer}).

create_emitter(Buffer) ->
  gen_server:call(?SERVER, {create_emitter, Buffer}).
destroy_emitter(Emitter) ->
  gen_server:cast(?SERVER, {destroy_emitter, Emitter}).

play(Emitter) ->
  gen_server:cast(?SERVER, {play, Emitter}).
stop(Emitter) ->
  gen_server:cast(?SERVER, {stop, Emitter}).

set_emitter_position(Emitter, X, Y) ->
  gen_server:cast(?SERVER, {set_emitter_position, Emitter, X, Y}).
set_emitter_velocity(Emitter, X, Y) ->
  gen_server:cast(?SERVER, {set_emitter_velocity, Emitter, X, Y}).
set_emitter_direction(Emitter, X, Y) ->
  gen_server:cast(?SERVER, {set_emitter_direction, Emitter, X, Y}).

set_listener_position(X, Y) ->
  gen_server:cast(?SERVER, {set_listener_position, X, Y}).
set_listener_velocity(X, Y) ->
  gen_server:cast(?SERVER, {set_listener_velocity, X, Y}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #molang_audio_state{}} | {ok, State :: #molang_audio_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  Port = erlang:open_port({spawn, ?LD_LIBRARY}, [binary]),
  {ok, #molang_audio_state{port = Port}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #molang_audio_state{}) ->
  {reply, Reply :: term(), NewState :: #molang_audio_state{}} |
  {reply, Reply :: term(), NewState :: #molang_audio_state{}, timeout() | hibernate} |
  {noreply, NewState :: #molang_audio_state{}} |
  {noreply, NewState :: #molang_audio_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #molang_audio_state{}} |
  {stop, Reason :: term(), NewState :: #molang_audio_state{}}).
handle_call({create_buffer, Filename}, _From, State = #molang_audio_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#00), erlang:term_to_binary(Filename)]),
  {reply, {ok, wait_data(Port)}, State};
handle_call({create_emitter, Buffer}, _From, State = #molang_audio_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#10), erlang:term_to_binary(Buffer)]),
  {reply, {ok, wait_data(Port)}, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #molang_audio_state{}) ->
  {noreply, NewState :: #molang_audio_state{}} |
  {noreply, NewState :: #molang_audio_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #molang_audio_state{}}).
handle_cast({destroy_buffer, Buffer}, State = #molang_audio_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#01), erlang:term_to_binary(Buffer)]),
  {noreply, State};
handle_cast({destroy_emitter, Emitter}, State = #molang_audio_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#11), erlang:term_to_binary(Emitter)]),
  {noreply, State};

handle_cast({play, Emitter}, State = #molang_audio_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#12), erlang:term_to_binary(Emitter)]),
  {noreply, State};
handle_cast({stop, Emitter}, State = #molang_audio_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#13), erlang:term_to_binary(Emitter)]),
  {noreply, State};

handle_cast({set_emitter_position, Emitter, X, Y}, State = #molang_audio_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#14), erlang:term_to_binary(Emitter), erlang:term_to_binary(X), erlang:term_to_binary(Y)]),
  {noreply, State};
handle_cast({set_emitter_velocity, Emitter, X, Y}, State = #molang_audio_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#15), erlang:term_to_binary(Emitter), erlang:term_to_binary(X), erlang:term_to_binary(Y)]),
  {noreply, State};
handle_cast({set_emitter_direction, Emitter, X, Y}, State = #molang_audio_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#16), erlang:term_to_binary(Emitter), erlang:term_to_binary(X), erlang:term_to_binary(Y)]),
  {noreply, State};

handle_cast({set_listener_position, Emitter, X, Y}, State = #molang_audio_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#20), erlang:term_to_binary(Emitter), erlang:term_to_binary(X), erlang:term_to_binary(Y)]),
  {noreply, State};
handle_cast({set_listener_velocity, Emitter, X, Y}, State = #molang_audio_state{port = Port}) ->
  erlang:port_command(Port, [erlang:term_to_binary(16#21), erlang:term_to_binary(Emitter), erlang:term_to_binary(X), erlang:term_to_binary(Y)]),
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #molang_audio_state{}) ->
  {noreply, NewState :: #molang_audio_state{}} |
  {noreply, NewState :: #molang_audio_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #molang_audio_state{}}).
handle_info(_Info, State = #molang_audio_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #molang_audio_state{}) -> term()).
terminate(_Reason, _State = #molang_audio_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #molang_audio_state{},
    Extra :: term()) ->
  {ok, NewState :: #molang_audio_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #molang_audio_state{}, _Extra) ->
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
