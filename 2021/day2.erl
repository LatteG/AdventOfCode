-module(day2).
-export([
	part1/0,
	part2/0
]).


-type direction() :: forward | down | up.
-record(instruction, {dir :: direction(), amnt :: integer()}).
-type instruction() :: #instruction{}.

-record(pos, {hor = 0 :: integer(),
			  depth = 0 :: integer(),
			  aim = 0 :: integer()}).
-type pos() :: #pos{}.


-spec part1() -> integer().
part1() ->
	InstructionStrings = aoc_utils:read_file("input\\day2.txt", lines),
	Instructions = lists:map(fun string_to_ins/1, InstructionStrings),
	FinalPos = lists:foldl(fun apply_instruction/2, #pos{}, Instructions),
	FinalPos#pos.hor * FinalPos#pos.depth.


-spec part2() -> integer().
part2() ->
	InstructionStrings = aoc_utils:read_file("input\\day2.txt", lines),
	Instructions = lists:map(fun string_to_ins/1, InstructionStrings),
	FinalPos = lists:foldl(fun apply_instruction_aim/2, #pos{}, Instructions),
	FinalPos#pos.hor * FinalPos#pos.depth.


-spec apply_instruction_aim(instruction(), pos()) -> pos().
apply_instruction_aim(#instruction{dir = forward, amnt = Amnt}, P) ->
	#pos{hor = P#pos.hor + Amnt,
		 depth = P#pos.depth + P#pos.aim * Amnt,
		 aim = P#pos.aim};
apply_instruction_aim(#instruction{dir = down, amnt = Amnt}, P) ->
	#pos{hor = P#pos.hor,
		 depth = P#pos.depth,
		 aim = P#pos.aim + Amnt};
apply_instruction_aim(#instruction{dir = up, amnt = Amnt}, P) ->
	#pos{hor = P#pos.hor,
		 depth = P#pos.depth,
		 aim = P#pos.aim - Amnt}.


-spec apply_instruction(instruction(), pos()) -> pos().
apply_instruction(#instruction{dir = forward, amnt = Amnt}, P) ->
	#pos{hor = P#pos.hor + Amnt,
		 depth = P#pos.depth};
apply_instruction(#instruction{dir = down, amnt = Amnt}, P) ->
	#pos{hor = P#pos.hor,
		 depth = P#pos.depth + Amnt};
apply_instruction(#instruction{dir = up, amnt = Amnt}, P) ->
	#pos{hor = P#pos.hor,
		 depth = P#pos.depth - Amnt}.


-spec string_to_ins(string()) -> instruction().
string_to_ins(Str) ->
	[DirStr, AmntStr] = string:split(Str, " "),
	#instruction{dir = string_to_dir(DirStr), amnt = list_to_integer(AmntStr)}.


-spec string_to_dir(string()) -> direction().
string_to_dir("forward") -> forward;
string_to_dir("up") -> up;
string_to_dir("down") -> down.
