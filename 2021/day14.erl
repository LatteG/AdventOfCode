-module(day14).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-define(parallel_size, 1000000).
-define(parallel_threshold, ?parallel_size * 1.5).


-type insertion_rules() :: #{string() => char()}.


-spec part1() -> integer().
part1() ->
	Lines = aoc_utils:read_file("input\\day14.txt", lines),
	{[InitialPolymer], [_ | RuleStrings]} = lists:split(1, Lines),
	InsertionRules = lists:foldl(fun parse_rule/2, maps:new(), RuleStrings),

	FinalPolymer = lists:foldl(fun(_, Polymer) ->
		iterate_polymer(InsertionRules, Polymer)
	end, InitialPolymer, lists:seq(1, 10)),

	{MostOccurances, LeastOccurances} = get_occurances(FinalPolymer),
	MostOccurances - LeastOccurances.


-spec part2() -> integer().
part2() ->
	Lines = aoc_utils:read_file("input\\day14.txt", lines),
	{[InitialPolymer], [_ | RuleStrings]} = lists:split(1, Lines),
	InsertionRules = lists:foldl(fun parse_rule/2, maps:new(), RuleStrings),

	FinalPolymer = lists:foldl(fun(It, Polymer) ->
		io:format("Iteration ~p, length: ~p...~n", [It, length(Polymer)]),
		iterate_polymer2(InsertionRules, Polymer)
	end, InitialPolymer, lists:seq(1, 40)),

	{MostOccurances, LeastOccurances} = get_occurances(FinalPolymer),
	MostOccurances - LeastOccurances.


-spec get_occurances(string()) -> {non_neg_integer(), non_neg_integer()}.
get_occurances(String) ->
	CharSet = sets:from_list(String),
	Occurances = sets:fold(fun(Char, Acc) ->
		[length(lists:filter(fun(C) -> C =:= Char end, String)) | Acc]
	end, [], CharSet),
	{lists:max(Occurances), lists:min(Occurances)}.


-spec iterate_polymer2(insertion_rules(), string()) -> string().
iterate_polymer2(Rules, Polymer) when length(Polymer) >= ?parallel_threshold ->
	FirstSegment = lists:sublist(Polymer, ?parallel_size),
	Parent = self(),
	Pid = spawn_link(fun() ->
		Head = iterate_polymer2(Rules, FirstSegment),
		Parent ! {self(), Head}
	end),
	[_ | Tail] = iterate_polymer2(Rules, lists:sublist(Polymer, ?parallel_size, length(Polymer))),
	receive
		{Pid, Head} -> Head ++ Tail
	end;
iterate_polymer2(Rules, Polymer = [PolymerHead | PolymerTail]) ->
	NewPolymerTail = lists:flatten(lists:map(fun({C0, C1}) ->
		[maps:get([C0, C1], Rules), C1]
	end, lists:zip(lists:droplast(Polymer), PolymerTail))),
	[PolymerHead | NewPolymerTail].


-spec iterate_polymer(insertion_rules(), string()) -> string().
iterate_polymer(_, Polymer = [_]) -> Polymer;
iterate_polymer(Rules, Polymer = [PolymerHead | PolymerTail]) ->
	[PolymerHead, maps:get(lists:sublist(Polymer, 2), Rules)] ++
	iterate_polymer(Rules, PolymerTail).


-spec parse_rule(string(), insertion_rules()) -> insertion_rules().
parse_rule(String, Map) ->
	[Key, [Value]] = string:split(String, " -> "),
	maps:put(Key, Value, Map).
