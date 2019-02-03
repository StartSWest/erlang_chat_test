%%%-------------------------------------------------------------------
%% @author Ivan Carmenates Garcia
%% @copyright (C) 2019
%%%-------------------------------------------------------------------
-module(chat_server_gs).
-behavior(gen_server).

-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(state, {
  database::maps
}).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

%%====================================================================
%% Gen-Server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Init.
%% @end
%%--------------------------------------------------------------------
-spec init(Args) ->
    {ok, State}
  | {ok, State, Timeout}
  | {ok, State, hibernate}
  | {ok, State, {continue, Continue}}
  | {stop, Reason}
  | ignore
 when
    Args::term(),
    State::term(),
    Timeout::timeout(),
    Continue::term(),
    Reason::term().

init(_Args) ->
  State = #state {
    database = #{
      users => ets:new(users, []),
      chats => ets:new(chats, []),
      groups => ets:new(groups, [])
    }
  },
  io:format("> Chat server started at: ~p.~n", [self()]),
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles call messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request, From, State) ->
    {reply, Reply, NewState, Timeout}
  | {reply, Reply, NewState, hibernate}
  | {reply, Reply, NewState, {continue, Continue}}
  | {noreply, NewState} | {noreply, NewState, Timeout}
  | {noreply, NewState, hibernate}
  | {noreply, NewState, {continue, Continue}}
  | {stop, Reason, Reply, NewState}
  | {stop, Reason, NewState}
 when
    Request::term(),
    Reply::term(),
    From::{pid(), Tag::term()},
    State::#state{},
    NewState::#state{},
    Timeout::timeout() | infinity,
    Continue::term(),
    Reason::term().

%% @private
%% Logs into the server.
handle_call({login, User}, _From = {ClientPid, _Tag}, State) ->
  % monitors the client process to know when it gets disconnected.  
  erlang:monitor(process, ClientPid),
  
  UsersTable = maps:get(users, State#state.database),
  true = ets:insert(UsersTable, {ClientPid, User}),
  
  ok = io:format("> A client got connected: ~p.~n", [ClientPid]),

  CurrentUsers = (e(qlc:q([U || {_, U} <- ets:table(UsersTable)]))),
  io:format("> Current users: ~p.~n", [CurrentUsers]),
  {reply, ok, State};

%% @private
%% Lists out the users in the server.
handle_call(list_users, _From, State) ->
  UsersTable = maps:get(users, State#state.database),
  R = e(qlc:q([User || {_, User} <- ets:table(UsersTable)])),
  {reply, R, State};

%% @private
%% Creates a chat.
handle_call({create_chat, User, TargetUser}, {FromPid, _Tag}, State) ->
  UsersTable = maps:get(users, State#state.database),
  case
    f(e(qlc:q([{P, U} || {P, U} <- ets:table(UsersTable), U == TargetUser])))
  of
    {TargetPid, TargetUser} ->
      ChatId = list_to_atom(lists:concat(lists:sort([User, TargetUser]))),
      ChatsTable = maps:get(chats, State#state.database),
      true = ets:insert(ChatsTable, {ChatId, User, TargetUser}),
      TargetPid ! {chat_request, User, ChatId},

      % sets monitor on both users.
      erlang:monitor(process, FromPid),
      erlang:monitor(process, TargetPid),
      {reply, {ok, {chat_id, ChatId}}, State};
    _ ->
      {reply, {error, user_not_connected}, State}
  end;

%% @private
%% Sends message to a user.
handle_call({send_msg, FromUser, ChatId, Message}, _From, State) ->
  ChatsTable = maps:get(chats, State#state.database),
  UsersTable = maps:get(users, State#state.database),

  R = case
    f(e(qlc:q([Chat || {Id, F, T} = Chat <- ets:table(ChatsTable),
               (Id == ChatId) and ((F == FromUser) or (T == FromUser))])))
  of
    {ChatId, F, T} = Chat ->
      % this is a normal chat.
      case Chat of
        {_, F, T} when F == FromUser ->
          TP = f(e(qlc:q([P || {P, U} <- ets:table(UsersTable), U == T]))),
          TP ! {message, F, Message},
          {ok, {message_sent_to, T}};
        {_, F, T} when T == FromUser ->
          FP = f(e(qlc:q([P || {P, U} <- ets:table(UsersTable), U == F]))),
          FP ! {message, T, Message},
          {ok, {message_sent_to, F}}
      end;
    _ ->
      {error, no_chat}
  end,
  {reply, R, State};

%% @private
%% Sends message to a group of users.
handle_call({send_group_msg, GroupName, FromUser, Message}, _From, State) ->
  GroupsTable = maps:get(groups, State#state.database),
  UsersTable = maps:get(users, State#state.database),

  R2 = case
    f(e(qlc:q([Group || {Name, _} = Group <- ets:table(GroupsTable),
               Name == GroupName])))
  of
    {GroupName, Users} ->
      UsersWithoutFromUser = Users -- [FromUser],
      case lists:member(FromUser, Users) of
        true ->
          case UsersWithoutFromUser of
            [] -> {error, empty_group};
            _ -> {ok, UsersWithoutFromUser}
          end;
        false -> {error, user_not_in_group}
      end;
    _ -> {error, no_group}
  end,

  R = case R2 of
    {ok, UsersWithoutFromUser2} ->
      % this is a group chat.
      lists:foreach(fun(User) ->
        case
          f(e(qlc:q([UserData || {_, U} = UserData <- ets:table(UsersTable),
                     U == User])))
        of
          {UP, User} -> UP ! {group_message, GroupName, FromUser, Message};
          Other ->
            io:format(Other)
        end
      end, UsersWithoutFromUser2),
      ok;
    {error, _} = Error ->
      Error
  end,
  {reply, R, State};

%% @private
%% Creates a chat group.
handle_call({create_group, GroupName, FromUser}, _From, State) ->
  GroupsTable = maps:get(groups, State#state.database),
  R = case
     f(e(qlc:q([Group || {Name, _} = Group <- ets:table(GroupsTable),
                Name == GroupName])))
  of
    {GroupName, _} -> ok;
    _ ->
      true = ets:insert(GroupsTable, {GroupName, [FromUser]}),
      {ok, {group_name, GroupName}}
  end,
  {reply, R, State};

%% @private
%% Adds a user to a chat group.
handle_call({add_to_group, GroupName, User}, _From, State) ->
  GroupsTable = maps:get(groups, State#state.database),
  UsersTable = maps:get(users, State#state.database),

  R = case
     f(e(qlc:q([Group || {Name, _} = Group <- ets:table(GroupsTable),
                Name == GroupName])))
  of
    {GroupName, Users} ->
      case lists:member(User, Users) of
        true -> ok;
        false ->
          case e(qlc:q([U || {_, U} <- ets:table(UsersTable), U == User])) of
            [] ->
              {error, user_not_connected};
            _ ->
              true = ets:insert(GroupsTable, {GroupName, [User | Users]}),
              {ok, added}
          end
      end;
    _ -> {error, no_group}
  end,
  {reply, R, State};

%% @private
%% Removes a user from a chat group.
handle_call({remove_from_group, GroupName, User}, _From, State) ->
  GroupsTable = maps:get(groups, State#state.database),
  R = case
     f(e(qlc:q([Group || {Name, _} = Group <- ets:table(GroupsTable),
                Name == GroupName])))
  of
    {GroupName, Users} ->
      case lists:member(User, Users) of
        true ->
          NewUsers = Users -- [User],
          case NewUsers of
            [] ->
              % if the group gets empty remove it.
              true = ets:delete(GroupsTable, GroupName);
            _ ->
              true = ets:insert(GroupsTable, {GroupName, NewUsers})
          end,
          {ok, removed};
        false ->
          ok
      end;
    _ -> {error, no_group}
  end,
  {reply, R, State};

handle_call(_Request, _From, State) ->
  Reply = not_implemented,
  NewState = State,
  {reply, Reply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request, State) ->
    {noreply, NewState}
  | {noreply, NewState, Timeout}
  | {noreply, NewState, hibernate}
  | {noreply, NewState, {continue, Continue}}
  | {stop, Reason, NewState}
 when
    Request::term(),
    State::#state{},
    NewState::#state{},
    Timeout::timeout(),
    Continue::term(),
    Reason::term().

handle_cast(_Request, State) ->
  NewState = State,
  {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles info messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info, State) ->
    {noreply, NewState}
  | {noreply, NewState, Timeout}
  | {noreply, NewState, hibernate}
  | {noreply, NewState, {continue, Continue}}
  | {stop, Reason, NewState}
 when
    Info::timeout | term(),
    State::term(),
    NewState::term(),
    Timeout::timeout(),
    Continue::term(),
    Reason::normal | term().

handle_info({'DOWN', _, process, ClientPid, _Reason}, State) ->
  GroupsTable = maps:get(groups, State#state.database), 
  ChatsTable = maps:get(chats, State#state.database),
  UsersTable = maps:get(users, State#state.database),
  
  % gets the user that went down.
  User = f(e(qlc:q([U || {P, U} <- ets:table(UsersTable), P == ClientPid]))),
  
  % removes group if it gets empty.
  case
    f(e(qlc:q([Group || {_, Users} = Group <- ets:table(GroupsTable),
               lists:member(User, Users)])))
  of
    {GroupId, Users} ->
      NewUsers = Users -- [User],
      case NewUsers of
        [] ->
          % if the group gets empty remove it.
          true = ets:delete(GroupsTable, GroupId);
        _ ->
          % updates the group with the users left.
          true = ets:insert(GroupsTable, {GroupId, NewUsers})
      end;
    _ ->
      ok
  end,

  % removes the chats associated to the user that went down.
  case
    f(e(qlc:q([Chat || {_, F, T} = Chat <- ets:table(ChatsTable),
               (F == User) or (T == User)])))
  of
    [] ->
      ok;
    {ChatId, _, _} = Chat ->
      case Chat of
        {_, F, T} when F == User ->
          % gets the client pid of the target user.
          TP = f(e(qlc:q([P || {P, U} <- ets:table(UsersTable), U == T]))),
          TP ! {user_disconnected, User};
        {_, F, T} when T == User ->
          FP = f(e(qlc:q([P || {P, U} <- ets:table(UsersTable), U == F]))),
          FP ! {user_disconnected, User}
      end,
      % removes the chat from the database if any of the users involved
      % gets disconnected.
      true = ets:delete(ChatsTable, ChatId)
  end,

  % removes the user from the database if its connection drops.
  true = ets:delete(UsersTable, ClientPid),
  io:format("> A client got disconnected: ~p.~n", [ClientPid]),
  
  CurrentUsers = (e(qlc:q([U || {_, U} <- ets:table(UsersTable)]))),
  io:format("> Current users: ~p.~n", [CurrentUsers]),
  {noreply, State};

handle_info(Info, State) ->
  io:format("~p", [Info]),
  NewState = State,
  {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles terminate messages.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) ->
    ok
 when
    Reason::normal | shutdown | {shutdown, term()} | term(),
    State::term().

terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles code change messages.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn, State, Extra) ->
    {ok, NewState}
  | {error, Reason}
 when
    OldVsn::Vsn | {down, Vsn},
    Vsn::term(),
    State::term(),
    NewState::term(),
    Extra::term(),
    Reason::term().

code_change(_OldVsn, State, _Extra) ->
  NewState = State,
  {ok, NewState}.

%%====================================================================
%% Private functions
%%====================================================================
e(Query) ->
  qlc:e(Query).

f(Result) ->
  case Result of
    [] -> [];
    R when is_list(R) -> hd(R);
    R -> R
  end.