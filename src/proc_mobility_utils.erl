%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michalwski@gmail.com>
%%% @copyright 2012 Michal Piotrowski
%%% @doc
%%% Additional function used by other modules. 
%%% @end
%%%-------------------------------------------------------------------
-module(proc_mobility_utils).

%%
%% Include files
%%
-include("proc_logging.hrl").
%%
%% Exported Functions
%%
-export([tcp_send_recv_reply/3, tcp_send/2]).

%%
%% API Functions
%%
%% @doc
%% Sends a message over TCP, waits for reply and replies to caller
-spec tcp_send_recv_reply({inet:ip_address() | inet:hostname(), inet:port_number()},
    any(), reference()) -> term().
tcp_send_recv_reply({Host, Port}, Message, ToReply) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 4}]) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, term_to_binary(Message)),
            ok = inet:setopts(Socket, [{active, once}]),
            receive
                {tcp, Socket, Bin} ->
                    %?INFO_MSG("Got ~p bytes from socket ~p", [byte_size(Bin), Socket]),
                    gen_server:reply(ToReply, binary_to_term(Bin));
                {tcp_closed, Socket} ->
                    ?INFO_MSG("socket ~p closed", [Socket]);
                {Error} ->
                    error_logger:error_msg("Got error ~p from socket ~p", [Error, Socket]),
                    gen_server:reply(ToReply, Error)
            end,
            ok = gen_tcp:close(Socket);
        Error ->
            gen_server:reply(ToReply, Error)
    end.
%% @doc Sends a message over TCP
-spec tcp_send({inet:ip_address() | inet:hostname(), inet:port_number()}, any()) -> no_return().
tcp_send({Host, Port}, Message) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Message)),
    ok = gen_tcp:close(Socket).

%%
%% Local Functions
%%

