%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_nifmq_bench).

-moduledoc """
Benchmark of QoS 0 local fan-out: the broker path (`emqx_broker:publish/1`, one mailbox
message per subscriber) against NifMQ (`emqx_nifmq:publish/1`, one stored message and a
native queue per subscriber).

Modes:
- `baseline`: subscribers call `emqx_broker:subscribe/1` and receive `#deliver{}`,
  batched with `emqx_utils:drain_deliver/1` the same way `emqx_connection` does.
- `nifmq_copy`: subscribers drain `#message{}` records, copied into their heap.
- `nifmq_view`: subscribers drain `{Topic, Payload}` pairs that refer to the stored message.

Scenarios:
- `idle`: the publisher sends the next message only when every subscriber has the previous
  one. Measures one fan-out from idle subscribers.
- `loaded`: up to `window` messages in flight. Measures throughput and CPU under load.
- `backlog`: subscribers are suspended, the publisher sends `K` messages, then the
  subscribers resume. Separates publish cost, memory per queued delivery and drain cost.

Each subscriber reads the publish time and sequence number from the first 16 payload bytes,
so every mode does the same per-message work.
""".

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_mqtt.hrl").
-include_lib("emqx_utils/include/emqx_message.hrl").

-export([start_emqx/1, run/1, matrix/2, main/1, drain_micro/2, main_micro/1]).

-define(DELIVERED, 1).
-define(TARGET, 2).
-define(BATCH, 100).
%% Deliveries per second in the paced scenarios.
-define(PACE, 300_000).
-define(SERIALIZE_OPTS, #{version => ?MQTT_PROTO_V5, max_size => ?MAX_PACKET_SIZE}).

%%------------------------------------------------------------------------------
%% Entry points
%%------------------------------------------------------------------------------

-doc "Start `emqx` with every listener disabled.".
start_emqx(WorkDir) ->
    Conf = """
    listeners.tcp.default.enable = false
    listeners.ssl.default.enable = false
    listeners.ws.default.enable = false
    listeners.wss.default.enable = false
    listeners.quic.default.enable = false
    """,
    emqx_cth_suite:start([{emqx, Conf}, emqx_nifmq], #{work_dir => WorkDir}).

-doc "Run from the command line: `main([OutCsv, Reps, Scenarios, Modes, Fanouts, Payloads])`.".
main([Out, Reps, Scenarios, Modes, Fanouts, Payloads]) ->
    WorkDir = filename:join(filename:dirname(Out), "bench-work-" ++ os:getpid()),
    _ = start_emqx(WorkDir),
    Split = fun(S) -> string:lexemes(S, ",") end,
    Spec = #{
        reps => list_to_integer(Reps),
        scenarios => [list_to_atom(S) || S <- Split(Scenarios)],
        modes => [list_to_atom(S) || S <- Split(Modes)],
        fanouts => [list_to_integer(S) || S <- Split(Fanouts)],
        payloads => [list_to_integer(S) || S <- Split(Payloads)]
    },
    matrix(Out, Spec),
    halt(0).

-doc """
Run every combination, repetitions innermost and modes interleaved, and append one CSV row
per run to `Out`.
""".
matrix(Out, Spec) ->
    #{reps := Reps, scenarios := Scs, modes := Modes, fanouts := Fs, payloads := Ps} = Spec,
    erlang:system_flag(scheduler_wall_time, true),
    ok = filelib:ensure_dir(Out),
    _ =
        case filelib:is_regular(Out) of
            true -> ok;
            false -> file:write_file(Out, [csv_header(), "\n"])
        end,
    _ = run(#{mode => nifmq_view, scenario => idle, fanout => 10, payload => 16}),
    [
        begin
            R = run(#{mode => M, scenario => Sc, fanout => F, payload => P, rep => Rep}),
            io:format("~s~n", [csv_row(R)]),
            ok = file:write_file(Out, [csv_row(R), "\n"], [append])
        end
     || Sc <- Scs, F <- Fs, P <- Ps, Rep <- lists:seq(1, Reps), M <- Modes
    ],
    ok.

%%------------------------------------------------------------------------------
%% One run
%%------------------------------------------------------------------------------

-doc "Run one combination and return the result map.".
run(Opts0) ->
    Opts = maps:merge(defaults(Opts0), Opts0),
    #{mode := Mode, scenario := Sc, fanout := N, payload := PSize} = Opts,
    Host = host_state(),
    Topic = iolist_to_binary(io_lib:format("bench/~p", [erlang:unique_integer([positive])])),
    A = atomics:new(2, [{signed, true}]),
    K = maps:get(k, Opts),
    Warm = maps:get(warm, Opts),
    Subs = start_subs(
        Mode, Topic, N, A, Warm, K + Warm, lists:member(Sc, [loaded_encode, paced_encode])
    ),
    Base = base_msg(Topic),
    Pad = binary:copy(<<"p">>, PSize - 16),
    %% Warm-up messages, not measured.
    pub_window(Mode, Base, Pad, 1, Warm, 1, N, A, 0),
    wait_delivered(Warm * N, A),
    erlang:garbage_collect(),
    Mem0 = mem_snapshot(),
    Res =
        case Sc of
            backlog -> run_backlog(Mode, Base, Pad, Warm, K, N, A, Subs, Mem0);
            _ -> run_flow(Mode, Base, Pad, Warm, K, N, A, window(Sc, Opts), Mem0)
        end,
    Lat = collect(Subs),
    stop_subs(Subs),
    wait_clean(Mode, Topic),
    maps:merge(maps:merge(Res, Host), Lat#{
        mode => Mode,
        scenario => Sc,
        fanout => N,
        payload => PSize,
        rep => maps:get(rep, Opts, 0),
        k => K,
        deliveries => K * N,
        all_deliveries => (K + Warm) * N,
        msg_words => erts_debug:flat_size(Base#message{payload = <<0:(PSize * 8)>>})
    }).

defaults(#{scenario := Sc, fanout := N}) ->
    Total =
        case Sc of
            idle -> 200_000;
            loaded -> 2_000_000;
            loaded_encode -> 1_000_000;
            paced -> 3 * ?PACE;
            paced_encode -> 3 * ?PACE;
            backlog -> 1_000_000
        end,
    K = max(Total div N, 20),
    Window =
        case Sc of
            idle -> 1;
            _ -> 32
        end,
    #{k => K, warm => max(K div 10, 2), window => Window}.

window(Sc, _Opts) when Sc =:= paced; Sc =:= paced_encode -> paced;
window(_Sc, Opts) -> maps:get(window, Opts).

run_flow(Mode, Base, Pad, Warm, K, N, A, Window, Mem0) ->
    Sampler = start_sampler(Mem0),
    S0 = sys_snapshot(),
    PubNs =
        case Window of
            paced ->
                IntervalNs = 1_000_000_000 * N div ?PACE,
                Start = erlang:monotonic_time(nanosecond),
                pub_paced(Mode, Base, Pad, Warm + 1, Warm + K, {Start, Warm + 1, IntervalNs}, 0);
            _ ->
                pub_window(Mode, Base, Pad, Warm + 1, Warm + K, Window, N, A, 0)
        end,
    wait_delivered((Warm + K) * N, A),
    S1 = sys_snapshot(),
    Peak = stop_sampler(Sampler),
    maps:merge(sys_delta(S0, S1, K * N), Peak#{pub_ns => PubNs}).

run_backlog(Mode, Base, Pad, Warm, K, N, A, Subs, Mem0) ->
    lists:foreach(fun erlang:suspend_process/1, Subs),
    S0 = sys_snapshot(),
    PubNs = pub_window(Mode, Base, Pad, Warm + 1, Warm + K, infinity, N, A, 0),
    S1 = sys_snapshot(),
    Mem1 = mem_snapshot(),
    lists:foreach(fun erlang:resume_process/1, Subs),
    wait_delivered((Warm + K) * N, A),
    S2 = sys_snapshot(),
    Pub = sys_delta(S0, S1, K * N),
    Drain = sys_delta(S1, S2, K * N),
    #{
        pub_ns => PubNs,
        wall_us => maps:get(wall_us, Pub) + maps:get(wall_us, Drain),
        cpu_ms => maps:get(cpu_ms, Pub) + maps:get(cpu_ms, Drain),
        cpu_ns_per_delivery => maps:get(cpu_ns_per_delivery, Pub) +
            maps:get(cpu_ns_per_delivery, Drain),
        sched_util => 0.0,
        sched_ns_per_delivery => maps:get(sched_ns_per_delivery, Pub) +
            maps:get(sched_ns_per_delivery, Drain),
        pub_sched_ns_per_delivery => maps:get(sched_ns_per_delivery, Pub),
        drain_sched_ns_per_delivery => maps:get(sched_ns_per_delivery, Drain),
        pub_cpu_ns_per_delivery => maps:get(cpu_ns_per_delivery, Pub),
        drain_cpu_ns_per_delivery => maps:get(cpu_ns_per_delivery, Drain),
        drain_wall_ns_per_delivery => maps:get(wall_us, Drain) * 1000 / (K * N),
        drain_sched_util => maps:get(sched_util, Drain),
        mem_delta => maps:get(mem, Mem1) - maps:get(mem, Mem0),
        rss_delta => maps:get(rss, Mem1) - maps:get(rss, Mem0),
        nif_entries => maps:get(nif_entries, Mem1),
        nif_queue_bytes => maps:get(nif_queue_bytes, Mem1) - maps:get(nif_queue_bytes, Mem0),
        nif_msgs => maps:get(nif_msgs, Mem1)
    }.

%%------------------------------------------------------------------------------
%% Drain micro-benchmark
%%------------------------------------------------------------------------------

-doc "Run from the command line: `main_micro([Out, Reps, Payloads])`.".
main_micro([Out, Reps, Payloads]) ->
    ok = filelib:ensure_dir(Out),
    Rows = [
        drain_micro(Mode, list_to_integer(P))
     || P <- string:lexemes(Payloads, ","),
        _ <- lists:seq(1, list_to_integer(Reps)),
        Mode <- [baseline, nifmq_copy, nifmq_view]
    ],
    Hdr = "mode,payload,msgs,fill_ns_per_msg,drain_ns_per_msg,gc_ns_per_msg,heap_words_per_msg\n",
    Lines = [
        io_lib:format("~p,~p,~p,~.1f,~.1f,~.1f,~.1f~n", [M, P, N, Fill, Drain, Gc, Heap])
     || #{mode := M, payload := P, msgs := N, fill := Fill, drain := Drain, gc := Gc, heap := Heap} <-
            Rows
    ],
    ok = file:write_file(Out, [Hdr | Lines]),
    io:put_chars([Hdr | Lines]),
    halt(0).

-doc """
Measure the drain side in one process, without scheduling. Fill M messages into the own
mailbox (baseline) or the own native queue (NifMQ), then drain them in batches of 100 and
keep them live, then run a full GC. The baseline receive leaves each message in a heap
fragment, so its copy into the heap is paid in the GC. The NifMQ copy drain builds the
term in the heap directly.
""".
drain_micro(Mode, PSize) ->
    Self = self(),
    Ref = make_ref(),
    spawn_link(fun() -> Self ! {Ref, drain_micro1(Mode, PSize, 100_000)} end),
    receive
        {Ref, Res} -> Res
    end.

drain_micro1(Mode, PSize, N) ->
    Topic = iolist_to_binary(io_lib:format("micro/~p", [erlang:unique_integer([positive])])),
    Base = base_msg(Topic),
    Pad = binary:copy(<<"p">>, PSize - 16),
    Msgs = [
        Base#message{id = emqx_guid:gen(), payload = <<0:64, I:64, Pad/binary>>}
     || I <- lists:seq(1, N)
    ],
    Q =
        case Mode of
            baseline ->
                undefined;
            _ ->
                Q0 = emqx_nifmq:new_queue(N),
                ok = emqx_nifmq:subscribe(Q0, Topic),
                Q0
        end,
    Me = self(),
    erlang:garbage_collect(),
    {Heap0, _} = heap_words(),
    T0 = erlang:monotonic_time(nanosecond),
    case Mode of
        baseline -> [Me ! #deliver{topic = Topic, message = M} || M <- Msgs];
        _ -> [emqx_nifmq:publish(M) || M <- Msgs]
    end,
    T1 = erlang:monotonic_time(nanosecond),
    receive
        nifmq_ready -> ok
    after 0 -> ok
    end,
    T2 = erlang:monotonic_time(nanosecond),
    Got = micro_drain(Mode, Q, []),
    T3 = erlang:monotonic_time(nanosecond),
    erlang:garbage_collect(),
    T4 = erlang:monotonic_time(nanosecond),
    {Heap1, _} = heap_words(),
    N = length(Got),
    _ = Msgs,
    #{
        mode => Mode,
        payload => PSize,
        msgs => N,
        fill => (T1 - T0) / N,
        drain => (T3 - T2) / N,
        gc => (T4 - T3) / N,
        heap => (Heap1 - Heap0) / N
    }.

micro_drain(baseline, _Q, Acc) ->
    case emqx_utils:drain_deliver(?BATCH) of
        [] -> lists:append(Acc);
        Ds -> micro_drain(baseline, undefined, [[M || #deliver{message = M} <- Ds] | Acc])
    end;
micro_drain(nifmq_copy, Q, Acc) ->
    case emqx_nifmq:drain(Q, ?BATCH) of
        {Msgs, true} -> micro_drain(nifmq_copy, Q, [Msgs | Acc]);
        {Msgs, false} -> lists:append([Msgs | Acc])
    end;
micro_drain(nifmq_view, Q, Acc) ->
    case emqx_nifmq:drain_view(Q, ?BATCH) of
        {Views, true} -> micro_drain(nifmq_view, Q, [Views | Acc]);
        {Views, false} -> lists:append([Views | Acc])
    end.

heap_words() ->
    {total_heap_size, W} = process_info(self(), total_heap_size),
    {W, 0}.

%%------------------------------------------------------------------------------
%% Publisher
%%------------------------------------------------------------------------------

base_msg(Topic) ->
    Msg = emqx_message:make(<<"bench-publisher">>, 0, Topic, <<>>),
    Msg#message{
        flags = #{dup => false, retain => false},
        headers = #{
            client_attrs => #{},
            peername => {{127, 0, 0, 1}, 50000},
            proto_ver => 5,
            protocol => mqtt,
            username => <<"bench-user">>,
            peerhost => {127, 0, 0, 1},
            properties => #{}
        }
    }.

%% Publish messages I..Last. Before message I, wait until (I - Window) messages reached
%% every subscriber. Return the total time spent inside the publish calls.
pub_window(_Mode, _Base, _Pad, I, Last, _Window, _N, _A, Acc) when I > Last ->
    Acc;
pub_window(Mode, Base, Pad, I, Last, Window, N, A, Acc) ->
    Window =/= infinity andalso wait_delivered((I - Window) * N, A),
    T0 = erlang:monotonic_time(nanosecond),
    Msg = Base#message{
        id = emqx_guid:gen(),
        payload = <<T0:64/signed, I:64, Pad/binary>>,
        timestamp = erlang:system_time(millisecond)
    },
    do_publish(Mode, Msg),
    T1 = erlang:monotonic_time(nanosecond),
    pub_window(Mode, Base, Pad, I + 1, Last, Window, N, A, Acc + T1 - T0).

%% Open loop: message I is due at Start + (I - First) * IntervalNs. The publisher sleeps
%% when it is more than 1 ms early, so messages leave in bursts of at most 1 ms.
pub_paced(_Mode, _Base, _Pad, I, Last, _Pace, Acc) when I > Last ->
    Acc;
pub_paced(Mode, Base, Pad, I, Last, {Start, First, IntervalNs} = Pace, Acc) ->
    Due = Start + (I - First) * IntervalNs,
    Early = Due - erlang:monotonic_time(nanosecond),
    Early > 1_000_000 andalso timer:sleep(Early div 1_000_000),
    T0 = erlang:monotonic_time(nanosecond),
    Msg = Base#message{
        id = emqx_guid:gen(),
        payload = <<T0:64/signed, I:64, Pad/binary>>,
        timestamp = erlang:system_time(millisecond)
    },
    do_publish(Mode, Msg),
    T1 = erlang:monotonic_time(nanosecond),
    pub_paced(Mode, Base, Pad, I + 1, Last, Pace, Acc + T1 - T0).

do_publish(baseline, Msg) ->
    emqx_broker:publish(Msg);
do_publish(_, Msg) ->
    emqx_nifmq:publish(Msg).

%% Wait until at least Need deliveries are counted. A subscriber that crosses the target
%% claims it with a compare-exchange and sends `go`.
wait_delivered(Need, _A) when Need =< 0 ->
    ok;
wait_delivered(Need, A) ->
    case atomics:get(A, ?DELIVERED) >= Need of
        true ->
            ok;
        false ->
            atomics:put(A, ?TARGET, Need),
            case atomics:get(A, ?DELIVERED) >= Need of
                true ->
                    case atomics:compare_exchange(A, ?TARGET, Need, 0) of
                        ok -> ok;
                        _ -> wait_go()
                    end;
                false ->
                    wait_go()
            end
    end.

wait_go() ->
    receive
        go -> ok
    after 120_000 -> error(bench_stalled)
    end.

%%------------------------------------------------------------------------------
%% Subscribers
%%------------------------------------------------------------------------------

start_subs(Mode, Topic, N, A, Warm, Last, Encode) ->
    Parent = self(),
    Pids = [
        spawn(fun() -> sub_init(Mode, Topic, Parent, A, Warm, Last, Encode) end)
     || _ <- lists:seq(1, N)
    ],
    [
        receive
            {sub_ready, P} -> ok
        end
     || P <- Pids
    ],
    Pids.

sub_init(Mode, Topic, Pub, A, Warm, Last, Encode) ->
    Q =
        case Mode of
            baseline ->
                ok = emqx_broker:subscribe(Topic),
                undefined;
            _ ->
                Q0 = emqx_nifmq:new_queue(Last + 1),
                ok = emqx_nifmq:subscribe(Q0, Topic),
                Q0
        end,
    Pub ! {sub_ready, self()},
    sub_loop(#{
        mode => Mode,
        q => Q,
        a => A,
        pub => Pub,
        warm => Warm,
        seq => 0,
        ooo => 0,
        batches => 0,
        hist => #{},
        encode => Encode,
        bytes => 0
    }).

sub_loop(#{mode := baseline} = S) ->
    receive
        #deliver{message = M} ->
            Ds = emqx_utils:drain_deliver(?BATCH - 1),
            Msgs = [M | [X || #deliver{message = X} <- Ds]],
            sub_loop(handle_batch([{T, P} || #message{topic = T, payload = P} <- Msgs], S));
        {report, From} ->
            report(From, S)
    end;
sub_loop(S) ->
    receive
        nifmq_ready ->
            sub_drain(S);
        {report, From} ->
            report(From, S)
    end.

sub_drain(#{mode := nifmq_copy, q := Q} = S) ->
    {Msgs, More} = emqx_nifmq:drain(Q, ?BATCH),
    sub_more(More, handle_batch([{T, P} || #message{topic = T, payload = P} <- Msgs], S));
sub_drain(#{mode := nifmq_view, q := Q} = S) ->
    {Views, More} = emqx_nifmq:drain_view(Q, ?BATCH),
    sub_more(More, handle_batch(Views, S)).

sub_more(true, S) -> sub_drain(S);
sub_more(false, S) -> sub_loop(S).

handle_batch([], S) ->
    S;
handle_batch(Payloads, #{a := A, pub := Pub} = S0) ->
    {S1, _Now} = lists:foldl(fun handle_one/2, {S0, undefined}, Payloads),
    S2 = encode_batch(Payloads, S1),
    S = S2#{batches := maps:get(batches, S2) + 1},
    New = atomics:add_get(A, ?DELIVERED, length(Payloads)),
    Target = atomics:get(A, ?TARGET),
    case Target > 0 andalso New >= Target of
        true ->
            case atomics:compare_exchange(A, ?TARGET, Target, 0) of
                ok -> Pub ! go;
                _ -> ok
            end;
        false ->
            ok
    end,
    S.

%% Latency is sampled on every 8th sequence number, the same messages in every mode.
%% The clock is read at most once per batch.
handle_one({_Topic, <<T0:64/signed, Seq:64, _/binary>>}, {S, Now0}) ->
    #{seq := Prev, ooo := Ooo, hist := H, warm := W} = S,
    Ooo1 =
        case Seq =:= Prev + 1 of
            true -> Ooo;
            false -> Ooo + 1
        end,
    case Seq > W andalso Seq rem 8 =:= 0 of
        true ->
            Now =
                case Now0 of
                    undefined -> erlang:monotonic_time(nanosecond);
                    _ -> Now0
                end,
            B = bucket(Now - T0),
            {S#{seq := Seq, ooo := Ooo1, hist := H#{B => maps:get(B, H, 0) + 1}}, Now};
        false ->
            {S#{seq := Seq, ooo := Ooo1}, Now0}
    end.

%% With `encode`, serialize each message as an MQTT 5.0 QoS 0 PUBLISH, as a connection
%% does before it writes to the socket.
encode_batch(_Payloads, #{encode := false} = S) ->
    S;
encode_batch(Payloads, #{bytes := Bytes0} = S) ->
    Bytes = lists:foldl(
        fun({Topic, Payload}, Acc) ->
            Pkt = #mqtt_packet{
                header = #mqtt_packet_header{type = ?PUBLISH, qos = ?QOS_0},
                variable = #mqtt_packet_publish{topic_name = Topic, properties = #{}},
                payload = Payload
            },
            Acc + iolist_size(emqx_frame:serialize_pkt(Pkt, ?SERIALIZE_OPTS))
        end,
        Bytes0,
        Payloads
    ),
    S#{bytes := Bytes}.

report(From, #{hist := H, ooo := Ooo, batches := Batches}) ->
    From ! {report, self(), H, Ooo, Batches},
    receive
        stop -> ok
    end.

collect(Subs) ->
    [P ! {report, self()} || P <- Subs],
    {H, Ooo, Batches} = lists:foldl(
        fun(P, {HAcc, OAcc, BAcc}) ->
            receive
                {report, P, H, O, B} ->
                    {
                        maps:fold(
                            fun(K, C, Acc) -> Acc#{K => maps:get(K, Acc, 0) + C} end, HAcc, H
                        ),
                        OAcc + O,
                        BAcc + B
                    }
            end
        end,
        {#{}, 0, 0},
        Subs
    ),
    maps:merge(percentiles(H), #{ooo => Ooo, batches => Batches}).

stop_subs(Subs) ->
    [P ! stop || P <- Subs],
    ok.

wait_clean(Mode, Topic) ->
    wait_clean(Mode, Topic, 500).

wait_clean(_Mode, _Topic, 0) ->
    error(cleanup_timeout);
wait_clean(Mode, Topic, N) ->
    Clean =
        case Mode of
            baseline -> ets:select_count(emqx_suboption, [{{{Topic, '_'}, '_'}, [], [true]}]) =:= 0;
            _ -> maps:get(live_msgs, emqx_nifmq:info()) =:= 0
        end,
    case Clean of
        true ->
            ok;
        false ->
            timer:sleep(20),
            wait_clean(Mode, Topic, N - 1)
    end.

%%------------------------------------------------------------------------------
%% Latency histogram: 8 buckets per power of two of nanoseconds (~9% resolution).
%%------------------------------------------------------------------------------

bucket(Ns) when Ns < 1 -> 0;
bucket(Ns) -> trunc(math:log2(Ns) * 8).

bucket_upper_us(B) -> math:pow(2, (B + 1) / 8) / 1000.

percentiles(H) when map_size(H) =:= 0 ->
    #{p50_us => 0.0, p99_us => 0.0, p999_us => 0.0, max_us => 0.0, samples => 0};
percentiles(H) ->
    Sorted = lists:sort(maps:to_list(H)),
    Total = lists:sum([C || {_, C} <- Sorted]),
    #{
        p50_us => pct(Sorted, Total * 0.5),
        p99_us => pct(Sorted, Total * 0.99),
        p999_us => pct(Sorted, Total * 0.999),
        max_us => bucket_upper_us(element(1, lists:last(Sorted))),
        samples => Total
    }.

pct([{B, C} | Rest], Need) ->
    case C >= Need orelse Rest =:= [] of
        true -> bucket_upper_us(B);
        false -> pct(Rest, Need - C)
    end.

%%------------------------------------------------------------------------------
%% System measurements
%%------------------------------------------------------------------------------

%% CPU of the whole VM (all threads) from /proc, in clock ticks of 10 ms.
proc_cpu_ms() ->
    {ok, Bin} = file:read_file("/proc/self/stat"),
    [_, Rest] = binary:split(Bin, <<") ">>),
    Fields = binary:split(Rest, <<" ">>, [global]),
    Utime = binary_to_integer(lists:nth(12, Fields)),
    Stime = binary_to_integer(lists:nth(13, Fields)),
    {Utime * 10, Stime * 10}.

rss_bytes() ->
    {ok, Bin} = file:read_file("/proc/self/status"),
    {match, [Kb]} = re:run(Bin, "VmRSS:\\s+(\\d+) kB", [{capture, all_but_first, binary}]),
    binary_to_integer(Kb) * 1024.

sys_snapshot() ->
    #{
        t => erlang:monotonic_time(microsecond),
        cpu => proc_cpu_ms(),
        sched => scheduler:get_sample_all()
    }.

sys_delta(
    #{t := T0, cpu := {U0, S0}, sched := Sc0}, #{t := T1, cpu := {U1, S1}, sched := Sc1}, Deliveries
) ->
    Cpu = (U1 - U0) + (S1 - S0),
    [{total, Util, _} | Per] = scheduler:utilization(Sc0, Sc1),
    %% Active time of normal and dirty CPU schedulers. Busy-wait spinning is not active.
    BusyNs = lists:sum([U * (T1 - T0) * 1000 || {Type, _, U, _} <- Per, Type =/= io]),
    #{
        wall_us => T1 - T0,
        sched_ns_per_delivery => BusyNs / Deliveries,
        cpu_ms => Cpu,
        cpu_user_ms => U1 - U0,
        cpu_sys_ms => S1 - S0,
        cpu_ns_per_delivery => Cpu * 1_000_000 / Deliveries,
        sched_util => Util
    }.

host_state() ->
    {ok, Load} = file:read_file("/proc/loadavg"),
    [L1 | _] = binary:split(Load, <<" ">>),
    {ok, Info} = file:read_file("/proc/meminfo"),
    {match, [Kb]} = re:run(Info, "MemAvailable:\\s+(\\d+) kB", [{capture, all_but_first, binary}]),
    #{host_load1 => binary_to_float(L1), host_avail_gb => binary_to_integer(Kb) div (1024 * 1024)}.

mem_snapshot() ->
    #{live_msgs := Msgs, live_entries := Entries} = emqx_nifmq:info(),
    #{queue_bytes := QBytes} = emqx_nifmq:info(),
    #{
        mem => erlang:memory(total),
        rss => rss_bytes(),
        nif_msgs => Msgs,
        nif_entries => Entries,
        nif_queue_bytes => QBytes
    }.

start_sampler(Mem0) ->
    Parent = self(),
    spawn_link(fun() -> sampler(Parent, Mem0, #{mem => 0, rss => 0, nif_entries => 0}) end).

sampler(Parent, Mem0, Peak) ->
    receive
        {stop, From} ->
            From ! {peak, Peak}
    after 100 ->
        M = mem_snapshot(),
        sampler(Parent, Mem0, #{
            mem => max(maps:get(mem, Peak), maps:get(mem, M) - maps:get(mem, Mem0)),
            rss => max(maps:get(rss, Peak), maps:get(rss, M) - maps:get(rss, Mem0)),
            nif_entries => max(maps:get(nif_entries, Peak), maps:get(nif_entries, M))
        })
    end.

stop_sampler(Pid) ->
    Pid ! {stop, self()},
    receive
        {peak, #{mem := Mem, rss := Rss, nif_entries := E}} ->
            #{mem_delta => Mem, rss_delta => Rss, nif_entries => E}
    end.

%%------------------------------------------------------------------------------
%% CSV
%%------------------------------------------------------------------------------

-define(COLS, [
    scenario,
    mode,
    fanout,
    payload,
    rep,
    k,
    deliveries,
    msg_words,
    wall_us,
    cpu_ms,
    cpu_ns_per_delivery,
    sched_ns_per_delivery,
    sched_util,
    pub_ns,
    pub_cpu_ns_per_delivery,
    drain_cpu_ns_per_delivery,
    pub_sched_ns_per_delivery,
    drain_sched_ns_per_delivery,
    drain_wall_ns_per_delivery,
    drain_sched_util,
    p50_us,
    p99_us,
    p999_us,
    max_us,
    samples,
    ooo,
    mem_delta,
    rss_delta,
    nif_entries,
    nif_msgs,
    nif_queue_bytes,
    batches,
    all_deliveries,
    host_load1,
    host_avail_gb
]).

csv_header() ->
    lists:join(",", [atom_to_list(C) || C <- ?COLS]).

csv_row(R) ->
    lists:join(",", [fmt(maps:get(C, R, "")) || C <- ?COLS]).

fmt(V) when is_float(V) -> float_to_list(V, [{decimals, 3}]);
fmt(V) when is_integer(V) -> integer_to_list(V);
fmt(V) when is_atom(V) -> atom_to_list(V);
fmt(V) -> V.
