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
    Req = case cowboy_req:method(Req0) of
              {Method, Req1} when Method =:= <<"PUT">>;
                                  Method =:= <<"POST">> ->
                  cowboy_req:set_resp_body(<<"42%">>, Req1);
              {_, Req1} -> Req1
          end,
    {true, Req, State}.

get_text_plain(Req, State) ->
    %% GET response body set like this
	{<<"43%">>, Req, State}.

is_complete(Req, State) ->
	{false, Req, State}.
