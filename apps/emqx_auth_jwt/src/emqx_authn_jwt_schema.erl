%%--------------------------------------------------------------------
%% Copyright (c) 2020-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_authn_jwt_schema).

-include("emqx_auth_jwt.hrl").
-include_lib("hocon/include/hoconsc.hrl").

-behaviour(emqx_authn_schema).

-export([
    fields/1,
    desc/1,
    refs/0,
    select_union_member/1,
    namespace/0
]).

namespace() -> authn.

refs() ->
    [
        ?R_REF(jwt_hmac),
        ?R_REF(jwt_public_key),
        ?R_REF(jwt_jwks)
    ].

select_union_member(#{<<"mechanism">> := ?AUTHN_MECHANISM_BIN} = Value) ->
    UseJWKS = maps:get(<<"use_jwks">>, Value, undefined),
    select_ref(boolean(UseJWKS), Value);
select_union_member(_Value) ->
    undefined.

select_ref(true, _) ->
    [?R_REF(jwt_jwks)];
select_ref(false, #{<<"public_key">> := _}) ->
    [?R_REF(jwt_public_key)];
select_ref(false, _) ->
    [?R_REF(jwt_hmac)];
select_ref(_, _) ->
    throw(#{
        field_name => use_jwks,
        expected => "true | false"
    }).

fields(jwt_hmac) ->
    [
        %% for hmac, it's the 'algorithm' field which selects this type
        %% use_jwks field can be ignored (kept for backward compatibility)
        {use_jwks,
            sc(
                hoconsc:enum([false]),
                #{
                    required => false,
                    desc => ?DESC(use_jwks),
                    importance => ?IMPORTANCE_HIDDEN
                }
            )},
        {algorithm,
            sc(hoconsc:enum(['hmac-based']), #{required => true, desc => ?DESC(algorithm)})},
        {secret, fun secret/1},
        {secret_base64_encoded, fun secret_base64_encoded/1}
    ] ++ common_fields();
fields(jwt_public_key) ->
    [
        %% for public-key, it's the 'algorithm' field which selects this type
        %% use_jwks field can be ignored (kept for backward compatibility)
        {use_jwks,
            sc(
                hoconsc:enum([false]),
                #{
                    required => false,
                    desc => ?DESC(use_jwks),
                    importance => ?IMPORTANCE_HIDDEN
                }
            )},
        {algorithm,
            sc(hoconsc:enum(['public-key']), #{required => true, desc => ?DESC(algorithm)})},
        {public_key, fun public_key/1}
    ] ++ common_fields();
fields(jwt_jwks) ->
    [
        {use_jwks, sc(hoconsc:enum([true]), #{required => true, desc => ?DESC(use_jwks)})},
        {endpoint, fun endpoint/1},
        {pool_size, fun emqx_connector_schema_lib:pool_size/1},
        {refresh_interval, fun refresh_interval/1},
        {ssl, #{
            type => hoconsc:ref(emqx_schema, "ssl_client_opts"),
            default => #{<<"enable">> => false},
            desc => ?DESC("ssl")
        }}
    ] ++ common_fields().

desc(jwt_hmac) ->
    ?DESC(jwt_hmac);
desc(jwt_public_key) ->
    ?DESC(jwt_public_key);
desc(jwt_jwks) ->
    ?DESC(jwt_jwks);
desc(undefined) ->
    undefined.

common_fields() ->
    [
        {mechanism, emqx_authn_schema:mechanism(?AUTHN_MECHANISM)},
        {acl_claim_name, #{
            type => binary(),
            default => <<"acl">>,
            desc => ?DESC(acl_claim_name)
        }},
        {verify_claims, fun verify_claims/1},
        {from, fun from/1}
    ] ++ emqx_authn_schema:common_fields().

secret(type) -> binary();
secret(desc) -> ?DESC(?FUNCTION_NAME);
secret(required) -> true;
secret(_) -> undefined.

secret_base64_encoded(type) -> boolean();
secret_base64_encoded(desc) -> ?DESC(?FUNCTION_NAME);
secret_base64_encoded(default) -> false;
secret_base64_encoded(_) -> undefined.

public_key(type) -> string();
public_key(desc) -> ?DESC(?FUNCTION_NAME);
public_key(required) -> ture;
public_key(_) -> undefined.

endpoint(type) -> string();
endpoint(desc) -> ?DESC(?FUNCTION_NAME);
endpoint(required) -> true;
endpoint(_) -> undefined.

refresh_interval(type) -> integer();
refresh_interval(desc) -> ?DESC(?FUNCTION_NAME);
refresh_interval(default) -> 300;
refresh_interval(validator) -> [fun(I) -> I > 0 end];
refresh_interval(_) -> undefined.

verify_claims(type) ->
    list();
verify_claims(desc) ->
    ?DESC(?FUNCTION_NAME);
verify_claims(default) ->
    [];
verify_claims(validator) ->
    [fun do_check_verify_claims/1];
verify_claims(converter) ->
    fun
        (VerifyClaims) when is_map(VerifyClaims) ->
            [{to_binary(K), V} || {K, V} <- maps:to_list(VerifyClaims)];
        (VerifyClaims) ->
            VerifyClaims
    end;
verify_claims(required) ->
    false;
verify_claims(_) ->
    undefined.

do_check_verify_claims([]) ->
    true;
do_check_verify_claims([{Name, Expected} | More]) ->
    check_claim_name(Name) andalso
        check_claim_expected(Expected) andalso
        do_check_verify_claims(More).

check_claim_name(exp) ->
    false;
check_claim_name(iat) ->
    false;
check_claim_name(nbf) ->
    false;
check_claim_name(Name) when
    Name == <<>>;
    Name == ""
->
    false;
check_claim_name(_) ->
    true.

check_claim_expected(Expected) ->
    try emqx_authn_jwt:handle_placeholder(Expected) of
        _ -> true
    catch
        _:_ ->
            false
    end.

from(type) -> hoconsc:enum([username, password]);
from(desc) -> ?DESC(?FUNCTION_NAME);
from(default) -> password;
from(_) -> undefined.

%% this field is technically a boolean type,
%% but union member selection is done before type casting (by typrefl),
%% so we have to allow strings too
boolean(<<"true">>) -> true;
boolean(<<"false">>) -> false;
boolean(Other) -> Other.

to_binary(A) when is_atom(A) ->
    atom_to_binary(A);
to_binary(B) when is_binary(B) ->
    B.

sc(Type, Meta) -> hoconsc:mk(Type, Meta).
