%%%-------------------------------------------------------------------
%%% @author  <tony@rogvall.se>
%%% @copyright (C) 2017, 
%%% @doc
%%%    Weather data publish server
%%% @end
%%% Created :  3 Apr 2017 by  <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(weatherpub_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(MAX_ERROR, 3).

-record(unit,
	{
	  temp_in_unit = "C",
	  temp_out_unit = "C",
	  pressure_unit = "hPa",
	  wind_unit = "km/h",
	  rain_unit = "mm"
	}).

-record(state, 
	{
	  hid,         %% hid weather station port
	  tref,
	  last  = [],  %% last sample data
	  unit  = #unit{}, 
	  fixed = [],  %% units etc
	  error = 0
	}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    State = open(#state{}),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({timeout,Ref,sample}, State) when Ref =:= State#state.tref ->
    case hid_weather_station:read_sample(State#state.hid) of
	{ok,Sample} ->
	    T = erlang:start_timer(1000*24, self(), sample),
	    publish(Sample, State#state.last),
	    {noreply, State#state { tref = T, last = Sample, error = 0 }};
	{error,Error} ->
	    lager:error("read error ~p", [Error]),
	    if State#state.error >= ?MAX_ERROR ->
		    hid_weather_station:close(State#state.hid),
		    T = erlang:start_timer(1000*5, self(), open),
		    {noreply, State#state { hid = undefined,
					    tref = T, error = 0 }};
	       true ->
		    T = erlang:start_timer(1000*10, self(), sample),
		    {noreply, State#state { tref = T,
					    error = State#state.error+1 }}
	    end
    end;
handle_info({timeout,Ref,open}, State) when Ref =:= State#state.tref ->	
    State1 = open(State),
    {noreply, State1};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

topic_map() ->
    #{ hum_in => <<"sensor.humidity.indoor">>,
       hum_out => <<"sensor.humidity.outdoor">>,
       temp_in => <<"sensor.temperature.indoor">>,
       temp_out => <<"sensor.temperature.outdoor">>,
       abs_pressure => <<"sensor.airpressure.abs">>,
       wind_ave => <<"sensor.wind.ave">>,
       wind_gust => <<"sensor.wind.gust">>,
       wind_dir => <<"sensor.wind.direction">>,
       rain => <<"sensor.rain.level">>
     }.

%% lookup unit givent the atom name
get_unit(hum_in,_Unit) -> {unit,"%"};
get_unit(hum_out,_Unit) -> {unit,"%"};
get_unit(temp_in,Unit) ->  {unit,Unit#unit.temp_in_unit};
get_unit(temp_out,Unit) -> {unit,Unit#unit.temp_out_unit};
get_unit(abs_pressure,Unit) -> {unit,Unit#unit.pressure_unit};
get_unit(wind_ave,Unit) -> {unit,Unit#unit.wind_unit};
get_unit(wind_gust,Unit) -> {unit,Unit#unit.wind_unit};
get_unit(wind_dir,_Unit) -> {unit,"degree"};
get_unit(rain,Unit) -> {unit,Unit#unit.rain_unit};
get_unit(_,_Unit) -> false.

publish_value(K,V,Topic,Last) ->
    case lists:keyfind(K, 1, Last) of
	{K,V} -> %% same value as last time (do not report)
	    ok;
	_ ->
	    xbus:pub(Topic,V)
    end.

publish([{K,V}|Ks], Last) ->
    case maps:find(K, topic_map()) of
	error ->
	    publish(Ks,Last);
	{ok,Topic} ->
	    publish_value(K,V,Topic,Last),
	    publish(Ks,Last)
    end;
publish([], _Last) ->
    ok.

%% Update the meta information when the units are known, that is
%% after reading from the weather station unit.
publish_meta([K|Ks], Unit) ->
    case get_unit(K, Unit) of
	false ->
	    publish_meta(Ks, Unit);
	{unit,U} ->
	    case maps:find(K, topic_map()) of
		{ok,Topic} ->
		    %% update meta with current unit (dynamic)
		    Meta0 = xbus:read_meta(Topic),
		    Meta = [{unit,U}|proplists:delete(unit, Meta0)],
		    xbus:pub_meta(Topic, Meta);
		error ->
		    ok
	    end,
	    publish_meta(Ks, Unit)
    end;
publish_meta([], _Unit) ->
    ok.

publish_meta(Unit) ->
    publish_meta(maps:keys(topic_map()), Unit).
	    
open(_State) ->
    case hid_weather_station:open() of
	{ok,Hid} ->
	    case hid_weather_station:read_fixed_16(Hid) of
		{ok,Fixed} ->
		    Unit = get_units(Fixed),
		    publish_meta(Unit),
		    T = erlang:start_timer(1000*4, self(), sample),
		    #state{ hid = Hid, fixed = Fixed, 
			    unit = Unit, tref = T };
		Error ->
		    lager:error("read error ~p, reopen in 5s", [Error]),
		    hid_weather_station:close(Hid),
		    T = erlang:start_timer(1000*5, self(), open),
		    #state { hid=undefined, tref = T }
	    end;
	Error ->
	    lager:error("open error ~p, reopen in 5s", [Error]),
	    T = erlang:start_timer(1000*5, self(), open),
	    #state { hid=undefined, tref = T }
    end.

get_units(Fixed) ->
    #unit {
       temp_in_unit =
	   proplists:get_value(temp_in_unit,Fixed,"C"),
       temp_out_unit =
	   proplists:get_value(temp_out_unit,Fixed,"C"),
       pressure_unit =
	   proplists:get_value(pressure_unit,Fixed,"hPa"),
       wind_unit =
	   proplists:get_value(wind_unit,Fixed,"km/h"),
       rain_unit = 
	   proplists:get_value(rain_unit,Fixed,"mm")
      }.
