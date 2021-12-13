-module(day13).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-record(coord, {x :: non_neg_integer(),
				y :: non_neg_integer()}).
-type coord() :: #coord{}.
-record(fold_instruction, {axis :: x | y,
						   coordinate :: non_neg_integer()}).
-type fold_instruction() :: #fold_instruction{}.
-type sheet() :: sets:set(coord()).


-spec part1() -> integer().
part1() ->
	Lines = aoc_utils:read_file("input\\day13.txt", lines),
	{CoordStrings, [_ | [FoldString | _]]} = lists:splitwith(fun(L) -> L =/= "" end, Lines),

	Sheet = sets:from_list(lists:map(fun parse_coord/1, CoordStrings)),
	FoldInstruction = parse_fold_instruction(FoldString),

	FoldedSheet = do_fold(FoldInstruction, Sheet),

	sets:size(FoldedSheet).


-spec part2() -> integer().
part2() ->
	Lines = aoc_utils:read_file("input\\day13.txt", lines),
	{CoordStrings, [_ | FoldStrings]} = lists:splitwith(fun(L) -> L =/= "" end, Lines),

	Sheet = sets:from_list(lists:map(fun parse_coord/1, CoordStrings)),
	FoldInstructions = lists:map(fun parse_fold_instruction/1, FoldStrings),

	FoldedSheet = lists:foldl(fun do_fold/2, Sheet, FoldInstructions),

	draw_sheet(FoldedSheet),

	sets:size(FoldedSheet).


-spec draw_sheet(sheet()) -> ok.
draw_sheet(Sheet) ->
	{Xs, Ys} = sets:fold(fun(#coord{x = X, y = Y}, {Xs, Ys}) ->
		{[X | Xs], [Y | Ys]}
	end, {[], []}, Sheet),
	MinX = lists:min(Xs),
	MaxX = lists:max(Xs),
	MinY = lists:min(Ys),
	MaxY = lists:max(Ys),

	lists:foreach(fun(Y) ->
		lists:foreach(fun(X) ->
			case sets:is_element(#coord{x = X, y = Y}, Sheet) of
				true  -> io:format("#");
				false -> io:format(" ")
			end
		end, lists:seq(MinX, MaxX)),
		io:format("~n")
	end, lists:seq(MinY, MaxY)).


-spec do_fold(fold_instruction(), sheet()) -> sheet().
do_fold(#fold_instruction{axis = x, coordinate = C}, Sheet) ->
	SheetWithoutCrease = sets:filter(fun(#coord{x = X}) -> X =/= C end, Sheet),
	sets:fold(fun(Coord = #coord{x = X, y = Y}, Acc) ->
		NewCoord = if
			X < C -> Coord;
			X > C -> #coord{x = mirrored(X, C), y = Y}
		end,
		sets:add_element(NewCoord, Acc)
	end, sets:new(), SheetWithoutCrease);
do_fold(#fold_instruction{axis = y, coordinate = C}, Sheet) ->
	SheetWithoutCrease = sets:filter(fun(#coord{y = Y}) -> Y =/= C end, Sheet),
	sets:fold(fun(Coord = #coord{x = X, y = Y}, Acc) ->
		NewCoord = if
			Y < C -> Coord;
			Y > C -> #coord{x = X, y = mirrored(Y, C)}
		end,
		sets:add_element(NewCoord, Acc)
	end, sets:new(), SheetWithoutCrease).


-spec mirrored(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
mirrored(Coordinate, Mirror) -> Mirror - (Coordinate - Mirror).


-spec parse_fold_instruction(string()) -> fold_instruction().
parse_fold_instruction(String) ->
	{_, FoldString} = string:take(String, "fold along"),
	[AxisString, CoordinateString] = string:split(FoldString, "="),
	#fold_instruction{axis = list_to_atom(AxisString),
					  coordinate = list_to_integer(CoordinateString)}.


-spec parse_coord(string()) -> coord().
parse_coord(String) ->
	[XString, YString] = string:split(String, ","),
	#coord{x = list_to_integer(XString),
		   y = list_to_integer(YString)}.
