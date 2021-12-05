-module(day5).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-record(coord, {x :: non_neg_integer(), y :: non_neg_integer()}).
-type coord() :: #coord{}.
-type direction() :: hor | ver | diag.
-record(line, {dir :: direction(), start_c :: coord(), end_c :: coord()}).
-type line() :: #line{}.
-type vent_map() :: maps:map(coord(), non_neg_integer()).


-spec part1() -> integer().
part1() ->
	LineStrings = aoc_utils:read_file("input\\day5.txt", lines),
	Lines = lists:map(fun parse_line/1, LineStrings),
	% io:format("Lines:~n~p~n", [Lines]),
	LinesNoDiag = lists:filter(fun(#line{dir = D}) -> D =/= diag end, Lines),
	Map = add_lines(LinesNoDiag),
	% print_map(Map),
	maps:size(maps:filter(fun(_, Val) -> Val > 1 end, Map)).


-spec part2() -> integer().
part2() ->
	LineStrings = aoc_utils:read_file("input\\day5.txt", lines),
	Lines = lists:map(fun parse_line/1, LineStrings),
	Map = add_lines(Lines),
	% print_map(Map),
	maps:size(maps:filter(fun(_, Val) -> Val > 1 end, Map)).


-spec add_lines(list(line())) -> vent_map().
add_lines(Lines) -> lists:foldl(fun add_line/2, maps:new(), Lines).


-spec print_map(vent_map()) -> ok.
print_map(Map) ->
	Keys = maps:keys(Map),
	{XCoords, YCoords} = lists:unzip(lists:map(fun(#coord{x = X, y = Y}) -> {X, Y} end, Keys)),
	MinX = lists:min(XCoords),
	MaxX = lists:max(XCoords),
	MinY = lists:min(YCoords),
	MaxY = lists:max(YCoords),
	MapStrings = lists:map(fun(Y) ->
		lists:foldl(fun(X, Acc) ->
			Acc ++ case maps:get(#coord{x = X, y = Y}, Map, empty) of
				empty -> ".";
				Num -> integer_to_list(Num)
			end
		end, "", lists:seq(MinX, MaxX))
	end, lists:seq(MinY, MaxY)),
	MapString = lists:flatten(lists:join($\n, MapStrings)),
	io:format("Map:~n~s~n", [MapString]).


-spec add_line(line(), vent_map()) -> vent_map().
add_line(#line{dir = ver, start_c = #coord{x = X, y = Y1}, end_c = #coord{y = Y2}}, Map) ->
	Coords = [#coord{x = X, y = Y} || Y <- lists:seq(min(Y1, Y2), max(Y1, Y2))],
	lists:foldl(fun add_coord/2, Map, Coords);
add_line(#line{dir = hor, start_c = #coord{x = X1, y = Y}, end_c = #coord{x = X2}}, Map) ->
	Coords = [#coord{x = X, y = Y} || X <- lists:seq(min(X1, X2), max(X1, X2))],
	lists:foldl(fun add_coord/2, Map, Coords);
add_line(#line{dir = diag, start_c = #coord{x = X1, y = Y1}, end_c = #coord{x = X2, y = Y2}}, Map) ->
	XDir = if
		X1 < X2 -> 1;
		X1 > X2 -> -1
	end,
	YDir = if
		Y1 < Y2 -> 1;
		Y1 > Y2 -> -1
	end,
	Coords = [#coord{x = X, y = Y} || {X, Y} <- lists:zip(lists:seq(X1, X2, XDir),
														  lists:seq(Y1, Y2, YDir))],
	lists:foldl(fun add_coord/2, Map, Coords).


-spec add_coord(coord(), vent_map()) -> vent_map().
add_coord(Coord, Map) -> maps:put(Coord, maps:get(Coord, Map, 0) + 1, Map).


-spec parse_line(string()) -> line().
parse_line(String) ->
	[StartString, EndString] = string:split(String, " -> "),
	StartCoord = parse_coord(StartString),
	EndCoord = parse_coord(EndString),
	Dir = case {StartCoord, EndCoord} of
		{#coord{x = X}, #coord{x = X}} -> ver;
		{#coord{y = Y}, #coord{y = Y}} -> hor;
		_ -> diag
	end,
	#line{dir = Dir, start_c = StartCoord, end_c = EndCoord}.


-spec parse_coord(string()) -> coord().
parse_coord(String) ->
	[X, Y] = lists:map(fun list_to_integer/1, string:split(String, ",")),
	#coord{x = X, y = Y}.
