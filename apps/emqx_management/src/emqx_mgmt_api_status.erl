%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-module(emqx_mgmt_api_status).
%% API
-behaviour(minirest_api).

-export([api_spec/0]).

-export([running_status/2]).

-include_lib("emqx/include/logger.hrl").

api_spec() ->
    {[status_api()], []}.

status_api() ->
    Path = "/status",
    Metadata = #{
        get => #{
            security => [],
            responses => #{
                <<"200">> => #{description => <<"running">>}}}},
    {Path, Metadata, running_status}.

running_status(get, _Request) ->
    {Status, Provided} = init:get_status(),
    ?SLOG(debug, #{msg => "init_progress", status => Status, provided => Provided}),
    AppStatus =
        case lists:keysearch(emqx, 1, application:which_applications()) of
            false         -> not_running;
            {value, _Val} -> running
        end,
    Body = io_lib:format("Node ~s status: ~s~n"
                         "Application status: ~s~n", [node(), Status, AppStatus]),
    HttpCode = case Status =:= started andalso AppStatus =:= running of
                   true -> 200;
                   false -> 503
               end,
    {HttpCode, #{<<"content-type">> => <<"text/plain">>}, iolist_to_binary(Body)}.
