%%--------------------------------------------------------------------
%% Copyright (c) 2022 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_license_schema).

-include_lib("typerefl/include/types.hrl").

%%------------------------------------------------------------------------------
%% hocon_schema callbacks
%%------------------------------------------------------------------------------

-behaviour(hocon_schema).

-export([roots/0, fields/1]).

roots() -> [{license, hoconsc:union(
                        [hoconsc:ref(?MODULE, file_license),
                         hoconsc:ref(?MODULE, value_license)])}].

fields(value_license) ->
    [{type, #{type => string,
              default => string}}]
    ++ common_fields();

fields(file_license) ->
    [{type, #{type => file,
              default => file}}]
    ++ common_fields().

common_fields() ->
    [ {value, string()}
    ].
