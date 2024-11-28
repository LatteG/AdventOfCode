-module(day20).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-define(one, $#).
-define(zero, $.).


-type image_enhancement() -> list(0 | 1).
-record(coord, {x :: integer(),
				y :: integer()}).
-type coord() :: #coord{}.
-type image() :: sets:set(coord()).


-spec part1() -> integer().
part1() ->
	Lines = aoc_utils:read_file("input\\day20_test.txt", lines),
	{EnhancementString, [_ | ImageString]} = lists:split(Lines, ""),
	-1.


-spec part2() -> integer().
part2() ->
	_ = aoc_utils:read_file("input\\day20_test.txt", lines),
	-1.
