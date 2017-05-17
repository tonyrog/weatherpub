%%% @author  <tony@rogvall.se>
%%% @copyright (C) 2017, 
%%% @doc
%%%    Start of weather station app
%%% @end
%%% Created :  3 Apr 2017 by  <tony@rogvall.se>

-module(weatherpub).

-export([start/0]).
-export([status/0]).

status() ->
    io:format("up\n", []).

start() ->
    application:ensure_all_started(weatherpub).

