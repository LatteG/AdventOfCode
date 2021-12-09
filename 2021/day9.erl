-module(day9).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-record(coord, {x :: non_neg_integer(), y :: non_neg_integer()}).
-type coord() :: #coord{}.
-type digit() :: 0..9.
-type height_map() :: list(list(digit())).
-type dir() :: up | down | left | right | wall | none.
-type dir_map() :: list(list(dir())).


-spec part1() -> integer().
part1() ->
	LineStrings = aoc_utils:read_file("input\\day9.txt", lines),
	Map = lists:map(fun string_to_digits/1, LineStrings),
	DirMap = get_dir_map(Map),
	DirVals = lists:zip(lists:flatten(DirMap), lists:flatten(Map)),
	{_, Vals} = lists:unzip(lists:filter(fun({D, _}) -> D =:= none end, DirVals)),
	lists:sum(Vals) + length(Vals).


-spec part2() -> integer().
part2() ->
	LineStrings = aoc_utils:read_file("input\\day9.txt", lines),
	Map = lists:map(fun string_to_digits/1, LineStrings),
	DirMap = get_dir_map(Map),
	LocalMinima = get_local_minima(DirMap),
	BasinSizes = lists:map(fun(LM) ->
		get_basin_size(sets:from_list([LM]), DirMap)
	end, LocalMinima),
	{ThreeLargest, _} = lists:split(3, lists:reverse(lists:sort(BasinSizes))),
	lists:foldl(fun(BS, Acc) -> Acc * BS end, 1, ThreeLargest).


-spec get_basin_size(sets:set(coord()), dir_map()) -> non_neg_integer().
get_basin_size(Basin, Map) ->
	NewBasin = sets:fold(fun(Coord, Acc) ->
		FlowingIn = get_flowing_in(Coord, Map),
		sets:union(Acc, FlowingIn)
	end, Basin, Basin),
	if
		NewBasin =:= Basin -> sets:size(Basin);
		NewBasin =/= Basin -> get_basin_size(NewBasin, Map)
	end.


-spec get_flowing_in(coord(), dir_map()) -> sets:set(coord()).
get_flowing_in(Coords, Map) ->
	NeighbourCoords = sets:from_list(get_neighbour_coords(Coords, Map)),
	sets:filter(fun(NC) ->
		get(NC, Map) =/= wall
	end, NeighbourCoords).


-spec get_local_minima(dir_map()) -> list(coord()).
get_local_minima(Map) ->
	AllCoords = lists:map(fun(Y) ->
		lists:map(fun(X) ->
			#coord{x = X, y = Y}
		end, lists:seq(1, length(lists:nth(1, Map))))
	end, lists:seq(1, length(Map))),
	DirCoords = lists:zip(lists:flatten(Map), lists:flatten(AllCoords)),
	{_, Coords} = lists:unzip(lists:filter(fun({D, _}) -> D =:= none end, DirCoords)),
	Coords.


-spec draw_dir_map(dir_map()) -> ok.
draw_dir_map(Map) ->
	MapString = string:join(lists:map(fun(Line) ->
		lists:map(fun dir_to_char/1, Line)
	end, Map), "\n"),
	io:format("~s~n", [MapString]).


-spec dir_to_char(dir()) -> char().
dir_to_char(none)  -> $O;
dir_to_char(left)  -> $<;
dir_to_char(right) -> $>;
dir_to_char(up)    -> $A;
dir_to_char(down)  -> $V;
dir_to_char(wall)  -> $=.


-spec get_dir_map(height_map()) -> dir_map().
get_dir_map(Map) ->
	MapWidth = length(lists:nth(1, Map)),
	lists:map(fun(Y) ->
		lists:map(fun(X) ->
			get_dir(#coord{x = X, y = Y}, Map)
		end, lists:seq(1, MapWidth))
	end, lists:seq(1, length(Map))).


-spec get_dir(coord(), height_map()) -> dir().
get_dir(Coord = #coord{x = X0, y = Y0}, Map) ->
	case get(Coord, Map) of
		9 -> wall;
		_ ->
			#coord{x = X1, y = Y1} = find_lowest_neighbour(Coord, Map),
			case {X0 - X1, Y0 - Y1} of
				{0, 0}  -> none;
				{0, 1}  -> up;
				{0, -1} -> down;
				{1, 0}  -> left;
				{-1, 0} -> right
			end
	end.


-spec find_lowest_neighbour(coord(), height_map()) -> coord().
find_lowest_neighbour(Coord, Map) ->
	Neighbours = get_neighbours(Coord, Map),
	{LowestCoord, _} = lists:foldl(fun({C, V}, {AC, AV}) ->
		if
			V =< AV -> {C, V};
			V > AV -> {AC, AV}
		end
	end, {Coord, get(Coord, Map)}, Neighbours),
	LowestCoord.


-spec get_neighbours(coord(), height_map()) -> list({coord(), digit()}).
get_neighbours(Coords, Map) ->
	NeighbourCoords = get_neighbour_coords(Coords, Map),
	NeighbourVals = lists:map(fun(C) -> get(C, Map) end, NeighbourCoords),
	lists:zip(NeighbourCoords, NeighbourVals).


-spec get_neighbour_coords(coord(), list(list())) -> list(coord()).
get_neighbour_coords(#coord{x = X0, y = Y0}, Map) ->
	[#coord{x = X, y = Y0} || X <- [X0 - 1, X0 + 1],
							  X >= 1,
							  X =< length(lists:nth(Y0, Map))]
	++
	[#coord{x = X0, y = Y} || Y <- [Y0 - 1, Y0 + 1],
					  		  Y >= 1,
							  Y =< length(Map)].


-spec get(coord(), list(list(T))) -> T.
get(#coord{x = X, y = Y}, Map) -> lists:nth(X, lists:nth(Y, Map)).


-spec string_to_digits(string()) -> list(digit()).
string_to_digits(String) -> lists:map(fun(C) -> list_to_integer([C]) end, String).
