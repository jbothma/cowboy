-module(rest_incomplete_resource).
-export([init/3, content_types_provided/2, get_text_plain/2, is_complete/2]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

get_text_plain(Req, State) ->
	{<<"42%">>, Req, State}.

is_complete(Req, State) ->
	{false, Req, State}.
