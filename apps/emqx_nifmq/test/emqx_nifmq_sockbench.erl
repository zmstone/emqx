%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_nifmq_sockbench).

-moduledoc """
Socket ceiling test for native delivery: one publisher, N subscribers over real TCP.

The broker node accepts N connections and runs one of these modes:

- `erlang`: one Erlang process per socket. The publisher sends `#deliver{}` to each process.
  The process collects up to `active_n` deliveries, keeps a QoS 1 window, encodes with
  `emqx_frame`, and calls `gen_tcp:send/2`. It parses PUBACKs with `emqx_frame`.
- `inline`: the native writer; the publisher's `emqx_nifmq_conn:dispatch/2` call encodes and
  writes to every socket. One Erlang process per socket parses PUBACKs and calls
  `emqx_nifmq_conn:puback/2`.
- `threads2`, `threads4`: the native writer with 2 or 4 poller threads that write.
- `inline_nr`, `threads2_nr`, `threads4_nr`: as above, and the poller threads also read the
  sockets and handle PUBACKs natively (`emqx_nifmq_conn:read_native/1`). The Erlang process
  of a socket wakes up only for other packets.

The client node opens the N connections, counts PUBLISH packets, checks order, records
latency from a timestamp in the payload, and answers QoS 1 with PUBACK.

Run it with `emqx_nifmq_sockbench.sh`.
""".

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_mqtt.hrl").

-export([main/1, client_main/0, client_start/5, client_reset/1, client_stats/1, client_stop/1]).

%% Each cell listens on the next port in 23890..23897, so the 4-tuples of a cell never
%% meet the TIME_WAIT sockets of the previous cell.
-define(PORT_BASE, 23890).
-define(TOPIC, <<"bench/t">>).
-define(ACTIVE_N, 10).
-define(WINDOW, 32).
-define(MAX_MQUEUE, 1000).
-define(WARMUP_MS, 1000).
-define(MEASURE_MS, 5000).
-define(SAMPLE_EVERY, 16).
-define(HIST_SIZE, 200).

%%------------------------------------------------------------------------------
%% Broker node
%%------------------------------------------------------------------------------

main([Out, Reps, Modes, Fanouts, Payloads, QoSs, Rates, ClientNode]) ->
    try
        run(
            Out,
            list_to_integer(Reps),
            [list_to_atom(M) || M <- split(Modes)],
            ints(Fanouts),
            ints(Payloads),
            ints(QoSs),
            ints(Rates),
            list_to_atom(ClientNode)
        ),
        erlang:halt(0)
    catch
        C:E:St ->
            io:format(standard_error, "~p:~p~n~p~n", [C, E, St]),
            erlang:halt(1)
    end.

split(S) -> string:tokens(S, ",").
ints(S) -> [list_to_integer(X) || X <- split(S)].

run(Out, Reps, Modes, Fanouts, Payloads, QoSs, Rates, Client) ->
    ok = wait_connect(Client, 50),
    erlang:system_flag(scheduler_wall_time, true),
    ok = file:write_file(Out, [lists:join(",", [atom_to_list(K) || K <- columns()]), "\n"]),
    Cells = [
        {R, M, F, P, Q, Rate}
     || R <- lists:seq(1, Reps), F <- Fanouts, P <- Payloads, Q <- QoSs, Rate <- Rates, M <- Modes
    ],
    lists:foldl(
        fun({R, M, F, P, Q, Rate}, I) ->
            Row = run_cell(Client, ?PORT_BASE + I rem 8, M, F, P, Q, Rate),
            Line = [to_s(maps:get(K, Row#{rep => R})) || K <- columns()],
            ok = file:write_file(Out, [lists:join(",", Line), "\n"], [append]),
            io:format("~p~n", [Row#{rep => R}]),
            I + 1
        end,
        0,
        Cells
    ),
    catch erpc:call(Client, init, stop, []),
    ok.

wait_connect(Client, 0) ->
    error({client_node_unreachable, Client});
wait_connect(Client, N) ->
    case net_kernel:connect_node(Client) of
        true ->
            ok;
        _ ->
            timer:sleep(200),
            wait_connect(Client, N - 1)
    end.

columns() ->
    [
        rep,
        mode,
        fanout,
        payload,
        qos,
        rate,
        deliveries,
        achieved_rate,
        cpu_ns_per_delivery,
        sched_ns_per_delivery,
        syscw_per_delivery,
        wakeups_per_delivery,
        vcsw_per_delivery,
        nvcsw_per_delivery,
        received,
        missing,
        ooo,
        p50_us,
        p99_us,
        p999_us,
        native_dropped,
        host_load1,
        host_avail_gb
    ].

to_s(V) when is_float(V) -> float_to_list(V, [{decimals, 3}]);
to_s(V) when is_integer(V) -> integer_to_list(V);
to_s(V) when is_atom(V) -> atom_to_list(V).

run_cell(Client, Port, Mode, Fanout, PayloadSize, QoS, Rate) ->
    ok = set_mode(Mode),
    {ok, L} = gen_tcp:listen(Port, [
        binary,
        {active, false},
        {reuseaddr, true},
        {backlog, 4096},
        {nodelay, true},
        {buffer, 4096},
        {high_watermark, 1 bsl 20},
        {send_timeout, 15000}
    ]),
    Wakeups = counters:new(1, [write_concurrency]),
    Coll = erpc:call(Client, ?MODULE, client_start, [{127, 0, 0, 1}, Port, Fanout, QoS, self()]),
    Subs = accept_n(L, Fanout, Mode, QoS, Wakeups, []),
    MsgRate = Rate / Fanout,
    Pub = spawn_link(fun() -> publisher(Mode, Subs, PayloadSize, QoS, MsgRate) end),
    timer:sleep(?WARMUP_MS),
    Host = host_state(),
    erpc:call(Client, ?MODULE, client_reset, [Coll]),
    P0 = pub_count(Pub),
    S0 = sys_snapshot(Wakeups),
    timer:sleep(?MEASURE_MS),
    S1 = sys_snapshot(Wakeups),
    P1 = pub_count(Pub),
    Deliveries = (P1 - P0) * Fanout,
    unlink(Pub),
    exit(Pub, kill),
    Stats = wait_client(Client, Coll, Deliveries, 0, 0),
    %% Close the broker side first, so TIME_WAIT stays on the broker's listen ports.
    [exit(S, kill) || S <- Subs],
    ok = gen_tcp:close(L),
    timer:sleep(500),
    ok = erpc:call(Client, ?MODULE, client_stop, [Coll]),
    maps:merge(
        maps:merge(Host, Stats),
        (sys_delta(S0, S1, Deliveries))#{
            mode => Mode,
            fanout => Fanout,
            payload => PayloadSize,
            qos => QoS,
            rate => Rate,
            deliveries => Deliveries
        }
    ).

set_mode(erlang) -> ok;
set_mode(inline) -> emqx_nifmq_conn:set_mode(inline, 1);
set_mode(threads2) -> emqx_nifmq_conn:set_mode(threads, 2);
set_mode(threads4) -> emqx_nifmq_conn:set_mode(threads, 4);
set_mode(inline_nr) -> set_mode(inline);
set_mode(threads2_nr) -> set_mode(threads2);
set_mode(threads4_nr) -> set_mode(threads4).

native_read(Mode) ->
    lists:member(Mode, [inline_nr, threads2_nr, threads4_nr]).

accept_n(_L, 0, _Mode, _QoS, _Wakeups, Acc) ->
    lists:reverse(Acc);
accept_n(L, N, Mode, QoS, Wakeups, Acc) ->
    {ok, S} = gen_tcp:accept(L, 30000),
    Self = self(),
    Pid = spawn(fun() ->
        receive
            go -> ok
        end,
        conn_init(Mode, S, QoS, Wakeups, Self)
    end),
    ok = gen_tcp:controlling_process(S, Pid),
    Pid ! go,
    receive
        {ready, Pid} -> ok
    after 30000 -> error(conn_not_ready)
    end,
    accept_n(L, N - 1, Mode, QoS, Wakeups, [Pid | Acc]).

%% Wait until the client has received every delivery of the window, or until it stops
%% making progress for one second.
wait_client(Client, Coll, Expected, Last, Stuck) ->
    Stats = #{received := Got} = erpc:call(Client, ?MODULE, client_stats, [Coll]),
    if
        Got >= Expected; Stuck >= 10 ->
            Stats;
        Got =:= Last ->
            timer:sleep(100),
            wait_client(Client, Coll, Expected, Got, Stuck + 1);
        true ->
            timer:sleep(100),
            wait_client(Client, Coll, Expected, Got, 0)
    end.

%%------------------------------------------------------------------------------
%% Publisher
%%------------------------------------------------------------------------------

pub_count(Pub) ->
    Ref = make_ref(),
    Pub ! {count, self(), Ref},
    receive
        {Ref, N} -> N
    after 10000 -> error(publisher_stuck)
    end.

publisher(Mode, Subs, PayloadSize, QoS, MsgRate) ->
    Pad = binary:copy(<<0>>, max(0, PayloadSize - 16)),
    T0 = erlang:monotonic_time(microsecond),
    pub_loop(Mode, Subs, Pad, QoS, MsgRate, T0, 0).

pub_loop(Mode, Subs, Pad, QoS, MsgRate, T0, Sent) ->
    receive
        {count, From, Ref} ->
            From ! {Ref, Sent},
            pub_loop(Mode, Subs, Pad, QoS, MsgRate, T0, Sent)
    after 1 ->
        Due = trunc((erlang:monotonic_time(microsecond) - T0) * MsgRate / 1_000_000),
        N = min(Due - Sent, 1000),
        ok = publish_n(Mode, Subs, Pad, QoS, Sent + 1, N),
        pub_loop(Mode, Subs, Pad, QoS, MsgRate, T0, Sent + max(N, 0))
    end.

publish_n(_Mode, _Subs, _Pad, _QoS, _Seq, N) when N =< 0 ->
    ok;
publish_n(Mode, Subs, Pad, QoS, Seq, N) ->
    Payload = <<Seq:64, (os:system_time(nanosecond)):64, Pad/binary>>,
    Msg = emqx_message:make(<<"pub">>, QoS, ?TOPIC, Payload),
    _ = publish(Mode, Subs, Msg),
    publish_n(Mode, Subs, Pad, QoS, Seq + 1, N - 1).

publish(erlang, Subs, Msg) ->
    Deliver = #deliver{topic = ?TOPIC, message = Msg},
    lists:foreach(fun(Pid) -> Pid ! Deliver end, Subs);
publish(_Native, _Subs, #message{topic = Topic, qos = QoS, from = From, payload = P} = Msg) ->
    emqx_nifmq_conn:dispatch(?TOPIC, #{
        topic => Topic,
        payload => P,
        qos => QoS,
        from => From,
        retain => emqx_message:get_flag(retain, Msg, false)
    }).

%%------------------------------------------------------------------------------
%% Broker connection processes
%%------------------------------------------------------------------------------

-record(ec, {sock, qos, wakeups, ps, sopts, inflight = #{}, q = queue:new(), next_id = 1}).

conn_init(erlang, S, QoS, Wakeups, Parent) ->
    ok = inet:setopts(S, [{active, ?ACTIVE_N}]),
    Parent ! {ready, self()},
    erl_loop(#ec{
        sock = S,
        qos = QoS,
        wakeups = Wakeups,
        ps = parse_state(),
        sopts = emqx_frame:serialize_opts(?MQTT_PROTO_V5, ?MAX_PACKET_SIZE)
    });
conn_init(Native, S, QoS, Wakeups, Parent) ->
    {ok, Fd} = inet:getfd(S),
    Conn = emqx_nifmq_conn:attach(Fd, #{
        proto_ver => 5,
        clientid => <<"sub">>,
        window => ?WINDOW,
        max_mqueue => ?MAX_MQUEUE,
        max_buffer_bytes => 1 bsl 20
    }),
    ok = emqx_nifmq_conn:subscribe(Conn, ?TOPIC, #{qos => QoS}),
    case native_read(Native) of
        true ->
            ok = emqx_nifmq_conn:read_native(Conn),
            Parent ! {ready, self()},
            nr_loop(S, Conn, Wakeups);
        false ->
            ok = inet:setopts(S, [{active, ?ACTIVE_N}]),
            Parent ! {ready, self()},
            nat_loop(S, Conn, Wakeups, parse_state())
    end.

parse_state() ->
    emqx_frame:initial_parse_state(#{version => ?MQTT_PROTO_V5, expect_connect => false}).

erl_loop(#ec{sock = S, wakeups = W} = St) ->
    receive
        #deliver{} = D ->
            counters:add(W, 1, 1),
            Ds = [D | emqx_utils:drain_deliver(?ACTIVE_N)],
            erl_loop(erl_send(lists:foldl(fun erl_deliver/2, {St, []}, Ds)));
        {tcp, S, Data} ->
            counters:add(W, 1, 1),
            {Ids, PS} = parse_pubacks(Data, St#ec.ps, []),
            St1 = lists:foldl(
                fun(Id, A) -> A#ec{inflight = maps:remove(Id, A#ec.inflight)} end,
                St#ec{
                    ps = PS
                },
                Ids
            ),
            erl_loop(erl_send(erl_refill(St1, [])));
        {tcp_passive, S} ->
            ok = inet:setopts(S, [{active, ?ACTIVE_N}]),
            erl_loop(St);
        {tcp_closed, S} ->
            ok
    end.

erl_deliver(#deliver{message = #message{qos = ?QOS_0} = Msg}, {St, Acc}) ->
    {St, [emqx_message:to_packet(undefined, Msg) | Acc]};
erl_deliver(#deliver{message = Msg}, {#ec{inflight = Inf, q = Q} = St, Acc}) ->
    case map_size(Inf) < ?WINDOW of
        true ->
            erl_send_qos1(Msg, St, Acc);
        false ->
            {St#ec{q = queue:in(Msg, Q)}, Acc}
    end.

erl_send_qos1(Msg, #ec{inflight = Inf, next_id = Id} = St, Acc) ->
    Next =
        case Id of
            65535 -> 1;
            _ -> Id + 1
        end,
    {St#ec{inflight = Inf#{Id => Msg}, next_id = Next}, [emqx_message:to_packet(Id, Msg) | Acc]}.

erl_refill(#ec{inflight = Inf, q = Q} = St, Acc) ->
    case map_size(Inf) < ?WINDOW andalso queue:out(Q) of
        {{value, Msg}, Q1} ->
            {St1, Acc1} = erl_send_qos1(Msg, St#ec{q = Q1}, Acc),
            erl_refill(St1, Acc1);
        _ ->
            {St, Acc}
    end.

erl_send({St, []}) ->
    St;
erl_send({#ec{sock = S, sopts = SOpts} = St, Packets}) ->
    Data = [emqx_frame:serialize_pkt(P, SOpts) || P <- lists:reverse(Packets)],
    _ = gen_tcp:send(S, Data),
    St.

nat_loop(S, Conn, W, PS) ->
    receive
        {tcp, S, Data} ->
            counters:add(W, 1, 1),
            {Ids, PS1} = parse_pubacks(Data, PS, []),
            lists:foreach(fun(Id) -> ok = emqx_nifmq_conn:puback(Conn, Id) end, Ids),
            nat_loop(S, Conn, W, PS1);
        {tcp_passive, S} ->
            ok = inet:setopts(S, [{active, ?ACTIVE_N}]),
            nat_loop(S, Conn, W, PS);
        {tcp_closed, S} ->
            emqx_nifmq_conn:close(Conn)
    end.

%% The socket stays passive: the native reader handles PUBACKs and the client sends
%% nothing else.
nr_loop(S, Conn, W) ->
    receive
        {nifmq_data, Conn, _Data} ->
            counters:add(W, 1, 1),
            nr_loop(S, Conn, W);
        {nifmq_closed, Conn, _Reason} ->
            emqx_nifmq_conn:close(Conn)
    end.

parse_pubacks(Data, PS, Acc) ->
    case emqx_frame:parse(Data, PS) of
        {#mqtt_packet{variable = #mqtt_packet_puback{packet_id = Id}}, Rest, PS1} ->
            parse_pubacks(Rest, PS1, [Id | Acc]);
        {_More, PS1} ->
            {lists:reverse(Acc), PS1}
    end.

%%------------------------------------------------------------------------------
%% Measurements
%%------------------------------------------------------------------------------

proc_cpu_ms() ->
    {ok, Bin} = file:read_file("/proc/self/stat"),
    [_, Rest] = binary:split(Bin, <<") ">>),
    Fields = binary:split(Rest, <<" ">>, [global]),
    (binary_to_integer(lists:nth(12, Fields)) + binary_to_integer(lists:nth(13, Fields))) * 10.

proc_syscw() ->
    {ok, Bin} = file:read_file("/proc/self/io"),
    {match, [N]} = re:run(Bin, "syscw: (\\d+)", [{capture, all_but_first, binary}]),
    binary_to_integer(N).

%% Voluntary and involuntary context switches, summed over all threads of the node.
proc_ctxsw() ->
    {ok, Tids} = file:list_dir("/proc/self/task"),
    lists:foldl(
        fun(Tid, {V, N}) ->
            case file:read_file(["/proc/self/task/", Tid, "/status"]) of
                {ok, Bin} ->
                    {V + status_field(Bin, "voluntary"), N + status_field(Bin, "nonvoluntary")};
                {error, _} ->
                    {V, N}
            end
        end,
        {0, 0},
        Tids
    ).

status_field(Bin, Name) ->
    {match, [X]} = re:run(Bin, ["\n", Name, "_ctxt_switches:\\s+(\\d+)"], [
        {capture, all_but_first, binary}
    ]),
    binary_to_integer(X).

native_dropped() ->
    try emqx_nifmq_conn:stats() of
        #{dropped_qos0 := D0, dropped_qos1 := D1} -> D0 + D1
    catch
        _:_ -> 0
    end.

sys_snapshot(Wakeups) ->
    #{
        t => erlang:monotonic_time(microsecond),
        cpu => proc_cpu_ms(),
        syscw => proc_syscw(),
        sched => scheduler:get_sample_all(),
        wakeups => counters:get(Wakeups, 1),
        ctxsw => proc_ctxsw(),
        dropped => native_dropped()
    }.

sys_delta(S0, S1, Deliveries0) ->
    Deliveries = max(Deliveries0, 1),
    #{t := T0, sched := Sc0} = S0,
    #{t := T1, sched := Sc1} = S1,
    [{total, _, _} | Per] = scheduler:utilization(Sc0, Sc1),
    BusyNs = lists:sum([U * (T1 - T0) * 1000 || {Type, _, U, _} <- Per, Type =/= io]),
    D = fun(K) -> maps:get(K, S1) - maps:get(K, S0) end,
    {V0, N0} = maps:get(ctxsw, S0),
    {V1, N1} = maps:get(ctxsw, S1),
    #{
        achieved_rate => Deliveries0 * 1_000_000 / (T1 - T0),
        cpu_ns_per_delivery => D(cpu) * 1_000_000 / Deliveries,
        sched_ns_per_delivery => BusyNs / Deliveries,
        syscw_per_delivery => D(syscw) / Deliveries,
        wakeups_per_delivery => D(wakeups) / Deliveries,
        vcsw_per_delivery => (V1 - V0) / Deliveries,
        nvcsw_per_delivery => (N1 - N0) / Deliveries,
        native_dropped => D(dropped)
    }.

host_state() ->
    {ok, Load} = file:read_file("/proc/loadavg"),
    [L1 | _] = binary:split(Load, <<" ">>),
    {ok, Info} = file:read_file("/proc/meminfo"),
    {match, [Kb]} = re:run(Info, "MemAvailable:\\s+(\\d+) kB", [{capture, all_but_first, binary}]),
    #{host_load1 => binary_to_float(L1), host_avail_gb => binary_to_integer(Kb) div (1024 * 1024)}.

%%------------------------------------------------------------------------------
%% Client node
%%------------------------------------------------------------------------------

client_main() ->
    receive
        stop -> ok
    end.

%% Counter indexes in the shared `counters` array.
-define(C_RECV, 1).
-define(C_MISSING, 2).
-define(C_OOO, 3).
-define(C_HIST0, 4).

client_start(Host, Port, N, QoS, _Broker) ->
    Parent = self(),
    Coll = spawn(fun() ->
        Cnt = counters:new(?C_HIST0 + ?HIST_SIZE, [write_concurrency]),
        Pids = [
            spawn(fun() -> client_conn(Host, Port, QoS, Cnt) end)
         || _ <- lists:seq(1, N)
        ],
        Parent ! {started, self()},
        coll_loop(Cnt, Pids)
    end),
    receive
        {started, Coll} -> Coll
    end.

coll_loop(Cnt, Pids) ->
    receive
        {reset, From} ->
            [counters:put(Cnt, I, 0) || I <- lists:seq(1, ?C_HIST0 + ?HIST_SIZE)],
            From ! {Cnt, ok},
            coll_loop(Cnt, Pids);
        {stats, From} ->
            From ! {Cnt, stats(Cnt)},
            coll_loop(Cnt, Pids);
        {stop, From} ->
            [exit(P, kill) || P <- Pids],
            From ! {Cnt, ok}
    end.

coll_call(Coll, Req) ->
    Coll ! {Req, self()},
    receive
        {_Cnt, Reply} -> Reply
    after 30000 -> error(collector_timeout)
    end.

client_reset(Coll) -> coll_call(Coll, reset).
client_stats(Coll) -> coll_call(Coll, stats).
client_stop(Coll) -> coll_call(Coll, stop).

stats(Cnt) ->
    Hist = [{I, counters:get(Cnt, ?C_HIST0 + I)} || I <- lists:seq(0, ?HIST_SIZE - 1)],
    Total = lists:sum([C || {_, C} <- Hist]),
    #{
        received => counters:get(Cnt, ?C_RECV),
        missing => counters:get(Cnt, ?C_MISSING),
        ooo => counters:get(Cnt, ?C_OOO),
        p50_us => pct(Hist, Total * 0.5),
        p99_us => pct(Hist, Total * 0.99),
        p999_us => pct(Hist, Total * 0.999)
    }.

%% Bucket I holds latencies in [1.1^I, 1.1^(I+1)) microseconds.
bucket(Us) when Us < 1 -> 0;
bucket(Us) -> min(?HIST_SIZE - 1, trunc(math:log(Us) / math:log(1.1))).

pct(_Hist, 0) ->
    0;
pct(_Hist, +0.0) ->
    0;
pct([{I, C} | Rest], Need) ->
    case C >= Need orelse Rest =:= [] of
        true -> round(math:pow(1.1, I + 1));
        false -> pct(Rest, Need - C)
    end.

client_conn(Host, Port, QoS, Cnt) ->
    {ok, S} = gen_tcp:connect(Host, Port, [binary, {active, 100}, {nodelay, true}], 30000),
    client_loop(S, QoS, Cnt, <<>>, 0, 0).

client_loop(S, QoS, Cnt, Buf, Last, K) ->
    receive
        {tcp, S, Data} ->
            Now = os:system_time(nanosecond),
            {Rest, Last1, K1, Acks} = client_parse(
                <<Buf/binary, Data/binary>>, Cnt, Now, Last, K, []
            ),
            _ = QoS > 0 andalso Acks =/= [] andalso gen_tcp:send(S, Acks),
            client_loop(S, QoS, Cnt, Rest, Last1, K1);
        {tcp_passive, S} ->
            ok = inet:setopts(S, [{active, 100}]),
            client_loop(S, QoS, Cnt, Buf, Last, K);
        {tcp_closed, S} ->
            ok
    end.

client_parse(<<B1, Rest0/binary>> = Buf, Cnt, Now, Last, K, Acks) ->
    case varint(Rest0, 1, 0) of
        {Len, Rest1} when byte_size(Rest1) >= Len ->
            <<Body:Len/binary, Rest/binary>> = Rest1,
            case B1 bsr 4 of
                ?PUBLISH ->
                    QoS = (B1 bsr 1) band 3,
                    {Seq, Ts, Acks1} = publish_body(Body, QoS, Acks),
                    {Last1, K1} = account(Cnt, Now, Seq, Ts, Last, K),
                    client_parse(Rest, Cnt, Now, Last1, K1, Acks1);
                _ ->
                    client_parse(Rest, Cnt, Now, Last, K, Acks)
            end;
        _ ->
            {Buf, Last, K, Acks}
    end;
client_parse(<<>>, _Cnt, _Now, Last, K, Acks) ->
    {<<>>, Last, K, Acks}.

publish_body(<<TLen:16, _T:TLen/binary, Rest/binary>>, 0, Acks) ->
    {_PropLen, <<Seq:64, Ts:64, _/binary>>} = skip_props(Rest),
    {Seq, Ts, Acks};
publish_body(<<TLen:16, _T:TLen/binary, Id:16, Rest/binary>>, _QoS, Acks) ->
    {_PropLen, <<Seq:64, Ts:64, _/binary>>} = skip_props(Rest),
    {Seq, Ts, [Acks, <<16#40, 2, Id:16>>]}.

skip_props(Bin) ->
    {Len, Rest} = varint(Bin, 1, 0),
    <<_:Len/binary, R/binary>> = Rest,
    {Len, R}.

varint(<<1:1, D:7, Rest/binary>>, M, Acc) -> varint(Rest, M * 128, Acc + D * M);
varint(<<0:1, D:7, Rest/binary>>, M, Acc) -> {Acc + D * M, Rest};
varint(<<>>, _M, _Acc) -> more.

account(Cnt, Now, Seq, Ts, Last, K) ->
    counters:add(Cnt, ?C_RECV, 1),
    if
        Last =:= 0; Seq =:= Last + 1 -> ok;
        Seq > Last + 1 -> counters:add(Cnt, ?C_MISSING, Seq - Last - 1);
        true -> counters:add(Cnt, ?C_OOO, 1)
    end,
    case K rem ?SAMPLE_EVERY of
        0 -> counters:add(Cnt, ?C_HIST0 + bucket((Now - Ts) div 1000), 1);
        _ -> ok
    end,
    {max(Seq, Last), K + 1}.
