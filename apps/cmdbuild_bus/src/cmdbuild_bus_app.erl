%%%-------------------------------------------------------------------
%% @doc cmdbuild_bus public API
%% @end
%%%-------------------------------------------------------------------

-module(cmdbuild_bus_app).

-behaviour(application).

-define(SERVER, ?MODULE).
-record(state, {name, timer}).

%% Application callbacks
-export([start_link/0, start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([ cmdb_to_es/0 ]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(_StartType, _StartArgs) ->
    cmdbuild_bus_sup:start_link().

init([]) ->
    Timer = erlang:send_after(1, self(), check),
    {ok, #state{timer=Timer}}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Local API user
%%====================================================================

%%
% Start command
%%
cmdb_to_es() ->
     gen_server:cast(?MODULE, {cmdb_to_es}).


%%====================================================================
%% Internal functions
%%====================================================================
handle_call({cmdb_to_es}, _From, State) ->
    {ok, ClassCI} = application:get_env(cmdbuild_bus, cmdbuild_sync_classes),
    {ok, SessionId} = request_cmdb({get_sessionid}),
    {ok, CIs}   = request_cmdb({get_cis, SessionId, ClassCI}),
    request_es({post_data, CIs}),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check, State) ->
    erlang:cancel_timer(State#state.timer),
    {ok, Ms} = application:get_env(cmdbuild_bus, sync_each_ms),
    cmdb_to_es(),
    Timer = erlang:send_after(Ms, self(), check),
    NewState = State#state{timer=Timer},
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Requesting Block
%%====================================================================
request_cmdb({get_cis, SessionId, ClassesCI }) ->
    %%
    % Request for list of CIs Classes
    %% 
    Method = get,
    {ok, Url} = application:get_env(cmdbuild_bus, cmdbuild_api),
    %%
    % Put Session ID to HttpHeader
    %% 
    HttpHeader = [
                    {"Content-Type", "application/json"},
                    {"CMDBuild-Authorization", SessionId}
                ],
    {ok, HttpOptions} = application:get_env(cmdbuild_bus, http_options),
    RequestOptions = [],
    %%
    % For each class in the CIs Classes List do following
    %%
    CIs = lists:foldl(
        fun(ClassCI, Acc) ->
            %%
            % Connotinate request for current CI Class
            %%
            NewUrl = Url ++ "/cmdbuild/services/rest/v2/classes/" ++ ClassCI ++ "/cards",
            %%
            % Do request for CI data
            %%
            { ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body} } = httpc:request(Method, {NewUrl, HttpHeader}, HttpOptions, RequestOptions),
            %%
            % Transform CI data to Elasticsearch format data and add it to Accumulator
            %%
            transform_to_es(Body) ++ Acc
        end,
        [],
        ClassesCI
    ),
    %%
    % Return results
    %%
    {ok, CIs }; 
request_cmdb({get_ci, SessionId, ClassCI }) ->
    %%
    % Request one class CI
    %% 
    Method = get,
    {ok, Url} = application:get_env(cmdbuild_bus, cmdbuild_api),
    %%
    % Connotinate request for current CI Class
    %%
    NewUrl = Url ++ "/cmdbuild/services/rest/v2/classes/" ++ ClassCI ++ "/cards",
    %%
    % Put Session ID to HttpHeader
    %% 
    HttpHeader = [
                    {"Content-Type", "application/json"},
                    {"CMDBuild-Authorization", SessionId}
                ],
    {ok, HttpOptions} = application:get_env(cmdbuild_bus, http_options),
    RequestOptions = [],
    %%
    % Do request for CI data
    %%
    Result = httpc:request(Method, {NewUrl, HttpHeader}, HttpOptions, RequestOptions),
    %%
    % Process result of requested CI data
    %%
    request_cmdb({ci, Result });
request_cmdb({ci, { ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body} } }) ->
    %%
    % If request result ok and HttpCode 200 transform data to Elasticsearch format data and return result
    %%
    CIs = transform_to_es(Body),
    {ok, CIs};
request_cmdb({get_sessionid}) ->
    %%
    % Request for session id
    %% 
    Method = post,
    {ok, Credentials} = application:get_env(cmdbuild_bus, cmdbuild_api_credentials),
    {ok, Url} = application:get_env(cmdbuild_bus, cmdbuild_api),
    NewUrl = Url ++ "/cmdbuild/services/rest/v2/sessions",
    HttpHeader = [{"Content-Type", "application/json"}],
    Type = [],
    Body = jiffy:encode(Credentials),
    HttpOptions = [],
    RequestOptions = [],
    Result = httpc:request(Method, {NewUrl, HttpHeader, Type, Body}, HttpOptions, RequestOptions),
    request_cmdb({sessionid, Result});
request_cmdb({sessionid, { ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body} } }) -> 
    %%
    % If request result ok and HttpCode 200 return session id
    %%
    {[{<<"data">>, {[_Username, _Role, _AvailableRoles, {<<"_id">>,SessionId}]}}]} = jiffy:decode(Body),
    {ok, binary_to_list(SessionId)}.


request_es({post_data, Data}) ->
    %%
    % Post data to Elasticsearch
    %% 
    Method = post,
    {ok, Url} = application:get_env(cmdbuild_bus, es_url_bulk),
    HttpHeader = [{"Content-Type", "application/x-ndjson"}],
    Type = [],
    Body = nl_separate_list(Data), % Separate all CIs by new line (required by Elasticsearch)
    {ok, HttpOptions} = application:get_env(cmdbuild_bus, http_options),
    RequestOptions = [],
    httpc:request(Method, {Url, HttpHeader, Type, Body}, HttpOptions, RequestOptions).


transform_to_es(CIs) ->
    %%
    % Transform CIs list to Elasticsearch Bulk format data
    %% 
    {[ {<<"data">>, CIList}, {<<"meta">>, _} ]} = jiffy:decode( erlang:iolist_to_binary(CIs) ),
    %%
    % Get Index and Type information from conf
    %% 
    {ok, IndexMap} = application:get_env(cmdbuild_bus, es_dst_index),
    IndexConf = maps:get(<<"_index">>, IndexMap),
    TypeConf = maps:get(<<"_type">>, IndexMap),
    %%
    % Process CI list and put result to Accumulator
    %% 
    lists:foldl(
                        fun(CI, Acc) ->
                            case CI of
                                {[  {<<"LastModifyTimeDiff">>, _},
                                    {<<"is_candidate_for_deletion">>, _},
                                    {<<"Description">>, _},
                                    {<<"_type">>, ItemType},
                                    {<<"id">>, EventId},
                                    {<<"_id">>, ItemId},
                                    {<<"LastModifyTime">>, _},
                                    {<<"Code">>, ItemName},
                                    {<<"Notes">>, _}
                                ]} -> 
                                    Index = binary_to_list( jiffy:encode(
                                                    #{ <<"index">> => #{ <<"_index">> => IndexConf, 
                                                                         <<"_type">>  => TypeConf, 
                                                                         <<"_id">>    => ItemId } 
                                                    }
                                            )),
                                    Data =  binary_to_list( jiffy:encode(
                                                    #{ <<"event_id">> => EventId,
                                                       <<"name">>     => ItemName,
                                                       <<"severity">> => -1,
                                                       <<"type">>     => list_to_binary( string:to_lower( binary_to_list(ItemType) ) )
                                                    }
                                            )),
                                        [ Index, Data | Acc ];
                                {[  {<<"LastModifyTimeDiff">>, _},
                                    {<<"is_candidate_for_deletion">>, _},
                                    {<<"Description">>, _},
                                    {<<"_type">>, ItemType},
                                    {<<"id">>, EventId},
                                    {<<"_id">>, ItemId},
                                    {<<"LastModifyTime">>, _},
                                    {<<"Code">>, ItemName},
                                    {<<"Application">>, _},
                                    {<<"Notes">>, _}
                                ]} ->
                                    Index = binary_to_list( jiffy:encode(
                                                    #{ <<"index">> => #{ <<"_index">> => IndexConf, 
                                                                         <<"_type">>  => TypeConf, 
                                                                         <<"_id">>    => ItemId } 
                                                    }
                                            )),
                                    Data =  binary_to_list( jiffy:encode(
                                                    #{ <<"event_id">> => EventId,
                                                       <<"name">>     => ItemName,
                                                       <<"severity">> => -1,
                                                       <<"type">>     => list_to_binary( string:to_lower( binary_to_list(ItemType) ) )
                                                    }
                                            )),
                                        [ Index, Data | Acc ];
                                {[  {<<"_type">>, ItemType},
                                    {<<"Description">>, _},
                                    {<<"_id">>, ItemId},
                                    {<<"Code">>, ItemName},
                                    {<<"Notes">>, _}
                                ]} ->
                                    Index = binary_to_list( jiffy:encode(
                                                    #{ <<"index">> => #{ <<"_index">> => IndexConf, 
                                                                         <<"_type">>  => TypeConf, 
                                                                         <<"_id">>    => ItemId } 
                                                    }
                                            )),
                                    Data =  binary_to_list( jiffy:encode(
                                                    #{ <<"event_id">> => 0,
                                                       <<"name">>     => ItemName,
                                                       <<"severity">> => -1,
                                                       <<"type">>     => list_to_binary( string:to_lower( binary_to_list(ItemType) ) )
                                                    }
                                            )),
                                        [ Index, Data | Acc ];
                                
                                Other -> io:format("~n", Other)
                            end
                        end,
                        [],
                        CIList
                    ).

nl_separate_list(List) ->
    %%
    % Separate CI ist by new line (required for Bulk in elasticsearch)
    %% 
    LineSep = io_lib:nl(),
    list_to_binary( [string:join(List, LineSep), LineSep] ).