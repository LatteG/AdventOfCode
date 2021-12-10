-module(day10).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-type left_bracket()  :: $( | $[ | ${ | $<.
-type right_bracket() :: $) | $] | $} | $>.
-type bracket() :: left_bracket() | right_bracket().


-spec part1() -> integer().
part1() ->
	Lines = aoc_utils:read_file("input\\day10.txt", lines),
	CorruptionStatus = lists:map(fun corrupted/1, Lines),
	% io:format("Corruption status:~n~p~n", [CorruptionStatus]),
	lists:foldl(fun({_, B}, Acc) ->
		Acc + bracket_to_error_score(B)
	end, 0, lists:filter(fun(CS) -> CS =/= no end, CorruptionStatus)).


-spec part2() -> integer().
part2() ->
	Lines = aoc_utils:read_file("input\\day10.txt", lines),
	CorruptionStatus = lists:map(fun corrupted/1, Lines),
	{IncompleteLines, _} = lists:unzip(lists:filter(fun({_, CS}) ->
		CS =:= no
	end, lists:zip(Lines, CorruptionStatus))),
	CompletionLines = lists:map(fun get_completion/1, IncompleteLines),
	CompletionScores = lists:sort(lists:map(fun get_completion_score/1, CompletionLines)),
	[{FinalScore, _}] = lists:filter(fun({A, B}) ->
		A =:= B
	end, lists:zip(CompletionScores, lists:reverse(CompletionScores))),
	FinalScore.


-spec get_completion_score(list(right_bracket())) -> non_neg_integer().
get_completion_score(Brackets) -> lists:foldl(fun(B, Acc) ->
	Acc * 5 + bracket_to_completion_score(B)
end, 0, Brackets).


-spec bracket_to_completion_score(right_bracket()) -> non_neg_integer().
bracket_to_completion_score($)) -> 1;
bracket_to_completion_score($]) -> 2;
bracket_to_completion_score($}) -> 3;
bracket_to_completion_score($>) -> 4.


-spec get_completion(list(bracket())) -> list(right_bracket()).
get_completion(Brackets) ->
	UnfinishedBrackets = get_completion(Brackets, []),
	lists:map(fun left_to_right_bracket/1, UnfinishedBrackets).


-spec get_completion(list(bracket()), list(left_bracket())) -> list(left_bracket()).
get_completion([], Stack) -> Stack;
get_completion([Bracket | BracketRest], Stack) ->
	case is_left_bracket(Bracket) of
		true -> get_completion(BracketRest, [Bracket | Stack]);
		false ->
			[_ | StackRest] = Stack,
			get_completion(BracketRest, StackRest)
	end.


-spec left_to_right_bracket(left_bracket()) -> right_bracket().
left_to_right_bracket($() -> $);
left_to_right_bracket(${) -> $};
left_to_right_bracket($[) -> $];
left_to_right_bracket($<) -> $>.


-spec bracket_to_error_score(right_bracket()) -> non_neg_integer().
bracket_to_error_score($)) -> 3;
bracket_to_error_score($]) -> 57;
bracket_to_error_score($}) -> 1197;
bracket_to_error_score($>) -> 25137.


-spec corrupted(list(bracket())) -> no | {yes, right_bracket()}.
corrupted(Line) -> corrupted(Line, []).


-spec corrupted(list(bracket()), list(left_bracket())) -> no | {yes, right_bracket()}.
corrupted([], _) -> no;
corrupted([Bracket | BracketRest], Stack) ->
	case is_left_bracket(Bracket) of
		true -> corrupted(BracketRest, [Bracket | Stack]);
		false ->
			[StackHead | StackRest] = Stack,
			case matching_brackets(StackHead, Bracket) of
				true -> corrupted(BracketRest, StackRest);
				false -> {yes, Bracket}
			end
	end.


-spec is_left_bracket(left_bracket()) -> true;
					 (right_bracket()) -> false.
is_left_bracket($() -> true;
is_left_bracket($[) -> true;
is_left_bracket(${) -> true;
is_left_bracket($<) -> true;
is_left_bracket(_) -> false.


-spec matching_brackets(left_bracket(), right_bracket()) -> boolean().
matching_brackets($(, $)) -> true;
matching_brackets($[, $]) -> true;
matching_brackets(${, $}) -> true;
matching_brackets($<, $>) -> true;
matching_brackets(_, _) -> false.
