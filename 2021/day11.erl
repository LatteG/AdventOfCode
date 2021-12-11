-module(day11).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-type octopus_map() :: list(list(0..9)).
-record(coord, {x :: non_neg_integer(),
				y :: non_neg_integer()}).
-type coord() :: #coord{}.


-spec part1() -> integer().
part1() ->
	Grid = aoc_utils:read_file("input\\day11.txt", digit_grid),
	model_energy_flow(Grid, 100, 0).


-spec part2() -> integer().
part2() ->
	Grid = aoc_utils:read_file("input\\day11.txt", digit_grid),
	find_first_synchronized_step(Grid, 1).


-spec print_grid(octopus_map()) -> ok.
print_grid(Grid) ->
	lists:foreach(fun(Line) ->
		lists:foreach(fun(Octopus) ->
			io:format("~p", [Octopus])
		end, Line),
		io:format("~n")
	end, Grid).


-spec find_first_synchronized_step(octopus_map(), non_neg_integer()) -> non_neg_integer().
find_first_synchronized_step(Grid, Step) ->
	IncrementedGrid = increment_grid(Grid),
	{FlashedGrid, _} = do_flashes(IncrementedGrid, sets:new()),
	SpentGrid = reset_flashed_octopuses(FlashedGrid),
	case is_synchronized(SpentGrid) of
		true -> Step;
		false -> find_first_synchronized_step(SpentGrid, Step + 1)
	end.


-spec is_synchronized(octopus_map()) -> boolean().
is_synchronized(Grid) ->
	lists:all(fun(Line) ->
		lists:all(fun(Octopus) ->
			Octopus =:= 0
		end, Line)
	end, Grid).


-spec model_energy_flow(octopus_map(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
model_energy_flow(_, 0, Flashes) -> Flashes;
model_energy_flow(Grid, Steps, Flashes) ->
	IncrementedGrid = increment_grid(Grid),
	{FlashedGrid, DeltaFlashes} = do_flashes(IncrementedGrid, sets:new()),
	SpentGrid = reset_flashed_octopuses(FlashedGrid),
	model_energy_flow(SpentGrid, Steps - 1, Flashes + DeltaFlashes).


-spec increment_grid(octopus_map()) -> list(list(1..10)).
increment_grid(Grid) ->
	lists:map(fun(Line) ->
		lists:map(fun(Elem) ->
			Elem + 1
		end, Line)
	end, Grid).


-spec reset_flashed_octopuses(list(list(non_neg_integer()))) -> octopus_map().
reset_flashed_octopuses(Grid) ->
	lists:map(fun(Line) ->
		lists:map(fun(Octopus) ->
			if
				Octopus >  9 -> 0;
				Octopus =< 9 -> Octopus
			end
		end, Line)
	end, Grid).


-spec do_flashes(Grid, sets:set(coord())) -> {Grid, non_neg_integer()}
	when
		Grid :: list(list(non_neg_integer())).
do_flashes(Grid, AlreadyFlashed) ->
	AllCoords = lists:map(fun(Y) ->
		lists:map(fun(X) ->
			#coord{x = X, y = Y}
		end, lists:seq(1, length(lists:nth(1, Grid))))
	end, lists:seq(1, length(Grid))),

	CoordVals = lists:zip(lists:flatten(AllCoords), lists:flatten(Grid)),
	{HighCoords, _} = lists:unzip(lists:filter(fun({_, V}) ->
		V > 9
	end, CoordVals)),

	FlashingCoords = sets:subtract(sets:from_list(HighCoords), AlreadyFlashed),

	case sets:is_empty(FlashingCoords) of
		true -> {Grid, sets:size(AlreadyFlashed)};
		false ->
			Neighbours = sets:fold(fun(C, Acc) ->
				Acc ++ get_neighbours(Grid, C)
			end, [], FlashingCoords),
			NewGrid = lists:foldl(fun(C, Acc) ->
				update_grid_elem(Acc, C, get(Acc, C) + 1)
			end, Grid, Neighbours),

			do_flashes(NewGrid, sets:union(AlreadyFlashed, FlashingCoords))
	end.


-spec update_grid_elem(list(list(T)), coord(), T) -> list(list(T)).
update_grid_elem(Grid, #coord{x = X, y = Y}, Elem) ->
	Line = lists:nth(Y, Grid),
	NewLine = lists:sublist(Line, X - 1) ++
			  [Elem] ++
			  lists:nthtail(X, Line),
	lists:sublist(Grid, Y - 1) ++
	[NewLine] ++
	lists:nthtail(Y, Grid).


-spec get_neighbours(list(list()), coord()) -> list(coord()).
get_neighbours(Grid, #coord{x = X0, y = Y0}) ->
	MaxX = length(lists:nth(1, Grid)),
	MaxY = length(Grid),
	[#coord{x = X, y = Y} || X <- lists:seq(max(X0 - 1, 1), min(X0 + 1, MaxX)),
							 Y <- lists:seq(max(Y0 - 1, 1), min(Y0 + 1, MaxY)),
							 (X =/= X0) orelse (Y =/= Y0)].


-spec get(list(list(T)), coord()) -> T.
get(Grid, #coord{x = X, y = Y}) -> lists:nth(X, lists:nth(Y, Grid)).
