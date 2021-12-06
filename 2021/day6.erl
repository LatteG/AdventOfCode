-module(day6).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-define(birth_cycle_start, 6).
-define(first_birth_cycle_start, 8).


-record(fish, {days_left :: non_neg_integer(), amnt :: non_neg_integer()}).
-type fish() :: #fish{}.


-spec part1() -> integer().
part1() ->
	Nums = aoc_utils:read_file("input\\day6.txt", ints),
	Fish = ints_to_fish(Nums),
	FinalFish = lists:foldl(fun(_, FishAcc) -> flatten_fish(fish_iterator(FishAcc)) end, Fish, lists:seq(1, 80)),
	fish_counter(FinalFish).


-spec part2() -> integer().
part2() ->
	Nums = aoc_utils:read_file("input\\day6.txt", ints),
	Fish = ints_to_fish(Nums),
	FinalFish = lists:foldl(fun(_, FishAcc) -> flatten_fish(fish_iterator(FishAcc)) end, Fish, lists:seq(1, 256)),
	fish_counter(FinalFish).


-spec flatten_fish(list(fish())) -> list(fish()).
flatten_fish([]) -> [];
flatten_fish(Fish) ->
	[#fish{days_left = DaysLeft} | _] = Fish,
	Amnt = fish_counter(lists:filter(fun(#fish{days_left = DL}) -> DL =:= DaysLeft end, Fish)),
	[#fish{days_left = DaysLeft, amnt = Amnt} | flatten_fish(lists:filter(fun(#fish{days_left = DL}) -> DL =/= DaysLeft end, Fish))].


-spec fish_counter(list(fish())) -> non_neg_integer().
fish_counter(Fish) -> lists:foldl(fun(#fish{amnt = A}, Acc) -> Acc + A end, 0, Fish).


-spec fish_iterator(list(fish())) -> list(fish()).
fish_iterator([]) -> [];
fish_iterator([#fish{days_left = 0, amnt = Amnt} | Rest]) ->
	[#fish{days_left = ?birth_cycle_start, amnt = Amnt} |
	[#fish{days_left = ?first_birth_cycle_start, amnt = Amnt} |
	fish_iterator(Rest)]];
fish_iterator([#fish{days_left = DaysLeft, amnt = Amnt} | Rest]) ->
	[#fish{days_left = DaysLeft - 1, amnt = Amnt} | fish_iterator(Rest)].


-spec ints_to_fish(list(non_neg_integer())) -> list(fish()).
ints_to_fish([]) -> [];
ints_to_fish(Nums) ->
	[Num | _] = Nums,
	Amnt = length(lists:filter(fun(N) -> N =:= Num end, Nums)),
	[#fish{days_left = Num, amnt = Amnt} | ints_to_fish(lists:filter(fun(N) -> N =/= Num end, Nums))].
