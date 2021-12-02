-module(day1).
-export([
	part1/0,
	part2/0
]).


-spec part1() -> integer().
part1() ->
	Measurements = lists:flatten(aoc_utils:read_file("input\\day1.txt", int_lines)),
	get_incr_amnt(Measurements).


-spec part2() -> integer().
part2() ->
	RawMeasurements = lists:flatten(aoc_utils:read_file("input\\day1.txt", int_lines)),
	Measurements = apply_sliding_window(RawMeasurements),
	get_incr_amnt(Measurements).


-spec apply_sliding_window(list(non_neg_integer())) -> list(non_neg_integer()).
apply_sliding_window([]) -> [];
apply_sliding_window([X]) -> [X];
apply_sliding_window([X1, X2]) -> [X1 + X2, X2];
apply_sliding_window([X1 | [X2 | [X3 | T]]]) ->
	[X1 + X2 + X3 | apply_sliding_window([X2 | [X3 | T]])].


-spec get_incr_amnt(list(non_neg_integer())) -> non_neg_integer().
get_incr_amnt(Measurements) ->
	[_ | MeasurementsTail] = Measurements,
	lists:foldl(fun({P, N}, Acc) -> Acc + get_incr(P, N) end, 0, lists:zip(lists:droplast(Measurements), MeasurementsTail)).


-spec get_incr(non_neg_integer(), non_neg_integer()) -> 1 | 0.
get_incr(P, N) when P < N -> 1;
get_incr(_, _) -> 0.
