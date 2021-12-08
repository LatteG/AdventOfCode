-module(day8).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-type mapping() ::#{char() => char()}.


-spec part1() -> integer().
part1() ->
	Lines = aoc_utils:read_file("input\\day8.txt", lines),
	{_, Signals} = lists:unzip(lists:map(fun parse_line/1, Lines)),
	count_simple_signals(Signals).


-spec part2() -> integer().
part2() ->
	Lines = aoc_utils:read_file("input\\day8.txt", lines),
	PatternSignalPairs = lists:map(fun parse_line/1, Lines),
	Signals = lists:map(fun get_signal_num/1, PatternSignalPairs),
	lists:sum(Signals).


-spec get_signal_num({list(string()), list(string())}) -> non_neg_integer().
get_signal_num({Patterns, Signals}) ->
	Mapping = get_mapping(Patterns),
	SignalDigits = lists:map(fun(S) -> parse_signal(Mapping, S) end, Signals),
	combine_digits(SignalDigits, 0).


-spec combine_digits(list(0..9), non_neg_integer()) -> non_neg_integer().
combine_digits([], Acc) -> Acc;
combine_digits([Digit | Rest], Acc) -> combine_digits(Rest, Acc * 10 + Digit).


-spec parse_signal(mapping(), string()) -> 0..9.
parse_signal(Map, Signal) ->
	CorrectedSignal = lists:sort(lists:map(fun(C) -> maps:get(C, Map) end, Signal)),
	case CorrectedSignal of
		"abcefg"  -> 0;
		"cf"      -> 1;
		"acdeg"   -> 2;
		"acdfg"   -> 3;
		"bcdf"    -> 4;
		"abdfg"   -> 5;
		"abdefg"  -> 6;
		"acf"     -> 7;
		"abcdefg" -> 8;
		"abcdfg"  -> 9
	end.


-spec get_mapping(list(string())) -> mapping().
get_mapping(PatternStrings) ->
	PatternSets = lists:map(fun sets:from_list/1, PatternStrings),
	[One] = lists:filter(fun(P) -> sets:size(P) =:= 2 end, PatternSets),
	[Four] = lists:filter(fun(P) -> sets:size(P) =:= 4 end, PatternSets),
	[Seven] = lists:filter(fun(P) -> sets:size(P) =:= 3 end, PatternSets),
	[Eight] = lists:filter(fun(P) -> sets:size(P) =:= 7 end, PatternSets),

	CF = One,
	A = sets:subtract(Seven, CF), % a
	BD = sets:subtract(Four, CF),
	EG = sets:subtract(Eight, sets:union([CF, A, BD])),

	[Nine] = lists:filter(fun(P) ->
		Diff = sets:subtract(P, sets:union(Four, Seven)),
		(sets:size(Diff) =:= 1) andalso
		sets:is_subset(sets:union(Four, Seven), P) andalso
		sets:is_subset(Diff, EG)
	end, PatternSets),
	G = sets:subtract(Nine, sets:union(Four, Seven)),
	E = sets:subtract(EG, G), % a, e, g

	[Five] = lists:filter(fun(P) ->
		Diff = sets:subtract(Nine, P),
		(sets:size(Diff) =:= 1) andalso
		sets:is_subset(P, Nine) andalso
		sets:is_subset(Diff, CF)
	end, PatternSets),
	C = sets:subtract(Nine, Five),
	F = sets:subtract(CF, C), % a, c, e, f, g

	[Three] = lists:filter(fun(P) ->
		Diff = sets:subtract(Nine, P),
		(sets:size(Diff) =:= 1) andalso
		sets:is_subset(P, Nine) andalso
		sets:is_subset(Diff, BD)
	end, PatternSets),
	B = sets:subtract(Nine, Three),
	D = sets:subtract(BD, B), % a, b, c, d, e, f, g

	Pattern = lists:flatten(lists:map(fun sets:to_list/1, [A, B, C, D, E, F, G])),
	maps:from_list(lists:zip(Pattern, "abcdefg")).


-spec count_simple_signals(list(list(string()))) -> non_neg_integer().
count_simple_signals([]) -> 0;
count_simple_signals([Signals | Rest]) ->
	lists:foldl(fun(S, Acc) ->
		Acc + case is_simple_signal(S) of
			true -> 1;
			false -> 0
		end
	end, 0, Signals) + count_simple_signals(Rest).


-spec is_simple_signal(string()) -> boolean().
is_simple_signal(S) -> lists:member(string:len(S), [2,3,4,7]).


-spec parse_line(string()) -> {list(string()), list(string())}.
parse_line(Line) ->
	[Pattern, Signal] = string:split(Line, " | "),
	{string:split(Pattern, " ", all), string:split(Signal, " ", all)}.
