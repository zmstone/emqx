%%--------------------------------------------------------------------
%% Copyright (c) 2022 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(emqx_mgmt_util_tests).

-include_lib("eunit/include/eunit.hrl").

kmg_test_() ->
    [
        ?_assertEqual(<<"1G">>, emqx_mgmt_util:kmg(1024 * 1024 * 1024)),
        ?_assertEqual(<<"1G">>, emqx_mgmt_util:kmg(1024 * 1024 * 1024 + 1)),
        ?_assertEqual(<<"1.01M">>, emqx_mgmt_util:kmg(1024 * 1024 + 1024 * 10)),
        ?_assertEqual(<<"1K">>, emqx_mgmt_util:kmg(1024))
    ].
