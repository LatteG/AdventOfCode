-module(day18).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-type num() :: {num(), num()} | integer().


-spec part1() -> integer().
part1() ->
	Lines = aoc_utils:read_file("input\\day18_test.txt", lines),
	{Nums = [Num | Rest], _} = lists:unzip(lists:map(fun parse_num/1, Lines)),
	io:format("Snailfish numbers:~n~p~n", [Nums]),
	Sum = lists:foldl(fun(N, Acc) ->
		snailfish_add(Acc, N)
	end, Num, Rest),
	io:format("Sum:~n~p~n", [Sum]),
	-1.


-spec part2() -> integer().
part2() ->
	_ = aoc_utils:read_file("input\\day18_test.txt", lines),
	-1.


-spec snailfish_add(num(), num()) -> num().
snailfish_add(Num1, Num2) -> snailfish_reduce({Num1, Num2}).


-spec snailfish_reduce(num()) -> num().
snailfish_reduce(Num) ->
	case explode(Num) of
		{explode, NewNum} -> snailfish_reduce(NewNum);
		cannot_explode ->
			case split(Num) of
				{split, NewNum} -> snailfish_reduce(NewNum);
				cannot_split -> Num
			end.
	end.


-spec explode(num()) -> {explode, num()} | cannot_explode.
explode(_) -> cannot_explode.


-spec split(num()) -> {split, num()} | cannot_split.
split(_) -> cannot_split.


-spec parse_num(string()) -> {num(), string()}.
parse_num([$[ | String]) ->
	{Left, LeftRest} = parse_num(String),
	{Right, Rest} = parse_num(LeftRest),
	{{Left, Right}, Rest};
parse_num([$, | String]) -> parse_num(String);
parse_num([$] | String]) -> parse_num(String);
parse_num(String) -> string:to_integer(String).
