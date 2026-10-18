%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_nifmq).

-moduledoc """
NifMQ prototype (experimental). QoS 0, local delivery only.

A publish stores the `#message{}` once in native memory and puts a reference to it in the
native queue of each subscriber of the exact topic. The queue owner receives the atom
`nifmq_ready` when its queue changes from "not notified" to "notified", and then calls
`drain/2` or `drain_view/2` until the result says no more messages are left.

This module runs beside the broker delivery path. Nothing in EMQX calls it.
""".

-include_lib("emqx_utils/include/emqx_message.hrl").

-export([
    new_queue/1,
    subscribe/2,
    unsubscribe/2,
    publish/1,
    drain/2,
    drain_view/2,
    set_congested/2,
    queue_info/1,
    info/0
]).

-export_type([queue/0]).

-opaque queue() :: reference().

-doc """
Create a queue owned by the calling process, with a cap of `Cap` messages.
A publish to a full queue drops the new message for that queue.
The queue unsubscribes itself and frees its messages when the owner exits.
""".
-spec new_queue(pos_integer()) -> queue().
new_queue(Cap) ->
    emqx_nifmq_nif:new_queue(Cap).

-doc "Subscribe the queue to an exact topic name. Wildcards are not supported.".
-spec subscribe(queue(), binary()) -> ok.
subscribe(Q, Topic) ->
    emqx_nifmq_nif:subscribe(Q, Topic).

-spec unsubscribe(queue(), binary()) -> ok.
unsubscribe(Q, Topic) ->
    emqx_nifmq_nif:unsubscribe(Q, Topic).

-doc """
Enqueue a QoS 0 message to every subscriber of its topic.
Return the number of queues that accepted the message.
Each NIF call serves at most 256 queues, so a large fan-out takes several calls.
""".
-spec publish(emqx_types:message()) -> non_neg_integer().
publish(#message{topic = Topic} = Msg) ->
    case emqx_nifmq_nif:publish(Topic, Msg) of
        {ok, N} -> N;
        {continue, Cont, N} -> publish_cont(Cont, N)
    end.

publish_cont(Cont, Acc) ->
    case emqx_nifmq_nif:publish_cont(Cont) of
        {ok, N} -> Acc + N;
        {continue, Cont, N} -> publish_cont(Cont, Acc + N)
    end.

-doc """
Take up to `Max` messages as `#message{}` records, oldest first.
`More` is `true` when messages are left. The caller must then drain again.
""".
-spec drain(queue(), pos_integer()) -> {[emqx_types:message()], boolean()}.
drain(Q, Max) ->
    emqx_nifmq_nif:drain(Q, Max).

-doc """
Take up to `Max` messages as `{Topic, Payload}` pairs, oldest first.
The binaries refer to the stored message and are not copied.
""".
-spec drain_view(queue(), pos_integer()) -> {[{binary(), binary()}], boolean()}.
drain_view(Q, Max) ->
    emqx_nifmq_nif:drain_view(Q, Max).

-doc """
Set or clear the congested flag. A congested queue accepts messages but sends no
notification. Clearing the flag returns `more` when messages are queued. The caller must
then drain.
""".
-spec set_congested(queue(), boolean()) -> ok | more.
set_congested(Q, Bool) ->
    emqx_nifmq_nif:set_congested(Q, Bool).

-doc "Return `{Len, Notified, Congested, Dropped}` for one queue.".
-spec queue_info(queue()) -> {non_neg_integer(), boolean(), boolean(), non_neg_integer()}.
queue_info(Q) ->
    emqx_nifmq_nif:queue_info(Q).

-doc "Return global counters of the native side.".
-spec info() -> #{atom() => integer()}.
info() ->
    emqx_nifmq_nif:info().
