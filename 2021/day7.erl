-module(day7).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-spec part1() -> integer().
part1() ->
	Crabs = aoc_utils:read_file("input\\day7.txt", ints),
	Pos = find_pos(Crabs, {lists:min(Crabs), lists:max(Crabs)}, fun get_fuel_cost/2),
	% io:format("Crabs: ~p~nPos: ~p~nFuel: ~p~n", [Crabs, Pos, get_fuel_cost(Crabs, Pos)]),
	get_fuel_cost(Crabs, Pos).


-spec part2() -> integer().
part2() ->
	Crabs = aoc_utils:read_file("input\\day7.txt", ints),
	Pos = find_pos(Crabs, {lists:min(Crabs), lists:max(Crabs)}, fun get_big_fuel_cost/2),
	% io:format("Crabs: ~p~nPos: ~p~nFuel: ~p~n", [Crabs, Pos, get_fuel_cost(Crabs, Pos)]),
	get_big_fuel_cost(Crabs, Pos).


-spec find_pos(Crabs, Bounds, CostFunction) -> non_neg_integer()
	when
		Crabs :: list(non_neg_integer()),
		Bounds :: {Pos, Pos},
		Pos :: non_neg_integer(),
		CostFunction :: fun((Crabs, Pos) -> non_neg_integer()).
find_pos(_, {Pos, Pos}, _) -> Pos;
find_pos(Crabs, {MinPos, MaxPos}, Cost) ->
	PivotLow = (MinPos + MaxPos) div 2,
	PivotHigh = PivotLow + 1,
	FuelLow = Cost(Crabs, PivotLow),
	FuelHigh = Cost(Crabs, PivotHigh),
	if
		FuelLow < FuelHigh -> find_pos(Crabs, {MinPos, PivotLow}, Cost);
		FuelLow > FuelHigh -> find_pos(Crabs, {PivotHigh, MaxPos}, Cost)
	end.


-spec get_fuel_cost(list(non_neg_integer()), non_neg_integer()) -> non_neg_integer().
get_fuel_cost(Crabs, Pos) -> lists:foldl(fun(Crab, Acc) -> Acc + abs(Pos - Crab) end, 0, Crabs).

-spec get_big_fuel_cost(list(non_neg_integer()), non_neg_integer()) -> non_neg_integer().
get_big_fuel_cost(Crabs, Pos) ->
	lists:foldl(fun(Crab, Acc) ->
		Delta = abs(Pos - Crab),
		Fuel = (Delta * (Delta + 1)) div 2,
		Acc + Fuel
	end, 0, Crabs).
