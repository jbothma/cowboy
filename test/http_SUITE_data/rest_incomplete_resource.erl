-module(rest_incomplete_resource).
-export([init/3,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_text_plain/2,
         get_text_plain/2,
         is_complete/2]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"PUT">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"text">>, <<"plain">>, '*'}, from_text_plain}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"plain">>, '*'}, get_text_plain}], Req, State}.

from_text_plain(Req0, State) ->
    %% POST response body is set like this
    Req2 = case cowboy_req:method(Req0) of
               {Method, Req1} when Method =:= <<"PUT">>;
                                   Method =:= <<"POST">> ->
                   cowboy_req:set_resp_body(<<"42%">>, Req1);
               {_, Req1} -> Req1
           end,
    case cowboy_req:body(Req2) of
        {ok, <<"give_created_location">>, Req3} ->
            {{true, <<"/rest_incomplete/newthing">>}, Req3, State};
        {ok, <<>>, Req3} ->
            {true, Req3, State}
    end.

get_text_plain(Req0, State) ->
    case cowboy_req:method(Req0) of
        {Method, Req1} when Method =:= <<"GET">> ->
            case cowboy_req:qs_val(<<"empty_response">>, Req1) of
                {<<"true">>, Req2} ->
                    {<<>>, Req2, State};
                {_, Req2} ->
                    {<<"43%">>, Req2, State}
            end;
        {_, Req1} ->
            {<<>>, Req1, State}
    end.

is_complete(Req, State) ->
	{false, Req, State}.
