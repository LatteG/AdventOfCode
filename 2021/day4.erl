-module(day4).
-export([
	part1/0,
	part2/0
]).


-define(marked, m).
-type board() :: list(list(?marked | non_neg_integer())).


-spec part1() -> integer().
part1() ->
	Lines = aoc_utils:read_file("input\\day4.txt", lines),
	{Numbers, Boards} = parse_input(Lines),
	{LastNumber, WinningBoard} = play_bingo(Numbers, Boards),
	BoardSum = get_board_sum(WinningBoard),
	LastNumber * BoardSum.


-spec part2() -> integer().
part2() ->
	Lines = aoc_utils:read_file("input\\day4.txt", lines),
	{Numbers, Boards} = parse_input(Lines),
	{LastNumber, LosingBoard} = find_loser(Numbers, Boards),
	BoardSum = get_board_sum(LosingBoard),
	LastNumber * BoardSum.


-spec get_board_sum(board()) -> non_neg_integer().
get_board_sum(Board) ->
	lists:sum(lists:filter(fun(E) -> E =/= ?marked end, lists:flatten(Board))).


-spec find_loser(list(non_neg_integer()), list(board())) -> {non_neg_integer(), board()}.
find_loser([Num | Rest], Boards) ->
	NewBoards = lists:map(fun(B) -> apply_number(Num, B) end, Boards),
	case lists:filter(fun(B) -> not is_winner(B) end, NewBoards) of
		[] ->
			[Board | _] = NewBoards,
			{Num, Board};
		FilteredBoards -> find_loser(Rest, FilteredBoards)
	end.


-spec play_bingo(list(non_neg_integer()), list(board())) -> {non_neg_integer(), board()}.
play_bingo([Num | Rest], Boards) ->
	NewBoards = lists:map(fun(B) -> apply_number(Num, B) end, Boards),
	case lists:filter(fun is_winner/1, NewBoards) of
		[] -> play_bingo(Rest, NewBoards);
		[Board | _] -> {Num, Board}
	end.


-spec apply_number(non_neg_integer(), board()) -> board().
apply_number(Num, Board) ->
	lists:map(fun(Line) ->
		lists:map(fun(Elem) ->
			case Elem of
				Num -> ?marked;
				_ -> Elem
			end
		end, Line)
	end, Board).


-spec is_winner(board()) -> boolean().
is_winner(Board) ->
	lists:any(fun(Line) ->
		lists:all(fun(Elem) -> Elem =:= ?marked end, Line)
	end, Board)
	orelse
	lists:any(fun(Line) ->
		lists:all(fun(Elem) -> Elem =:= ?marked end, Line)
	end, lists:map(fun(N) ->
		lists:map(fun(Line) ->
			lists:nth(N, Line)
		end, Board)
	end, lists:seq(1, 5))).



-spec parse_input(list(string())) -> {list(non_neg_integer()), list(board())}.
parse_input(Lines) ->
	{[NumbersString], BoardsString} = lists:split(1, Lines),
	Numbers = lists:map(fun list_to_integer/1, string:split(NumbersString, ",", all)),
	Boards = parse_boards(BoardsString),
	{Numbers, Boards}.


-spec parse_boards(list(string())) -> list(board()).
parse_boards([]) -> [];
parse_boards(["" | Strings]) ->
	{BoardStrings, Rest} = lists:split(5, Strings),
	Board = lists:map(fun(Line) ->
		SplitLine = string:split(Line, " ", all),
		FilteredLine = lists:filter(fun(E) -> E =/= [] end, SplitLine),
		lists:map(fun list_to_integer/1, FilteredLine)
	end, BoardStrings),
	[Board | parse_boards(Rest)].
