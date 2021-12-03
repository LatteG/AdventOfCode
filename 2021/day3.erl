-module(day3).
-export([
	part1/0,
	part2/0
]).


-spec part1() -> integer().
part1() ->
	BinaryStrings = aoc_utils:read_file("input\\day3.txt", lines),
	Binaries = lists:map(fun(BS) ->
		lists:map(fun(BC) ->
			case BC of
				$1 -> 1;
				$0 -> 0
			end
		end, BS)
	end, BinaryStrings),
	[FirstBin | _] = Binaries,
	BinAmnt = length(Binaries),
	BinLen = length(FirstBin),

	{GammaCounts, EpsilonCounts} = lists:foldl(fun count_ones_and_zeroes/2, {lists:duplicate(BinLen, 0), lists:duplicate(BinLen, 0)}, Binaries),

	{GammaBinary, EpsilonBinary} = lists:unzip(get_gamma_and_epsilon(lists:zip(GammaCounts, EpsilonCounts), BinAmnt div 2)),

	GammaRate = parse_binary(GammaBinary),
	EpsilonRate = parse_binary(EpsilonBinary),

	GammaRate * EpsilonRate.


-spec part2() -> integer().
part2() ->
	BinaryStrings = aoc_utils:read_file("input\\day3.txt", lines),
	Binaries = lists:map(fun(BS) ->
		lists:map(fun(BC) ->
			case BC of
				$1 -> 1;
				$0 -> 0
			end
		end, BS)
	end, BinaryStrings),

	OxygenRatingBinary = find_rating(Binaries, [], fun(A, B) -> A >= B end),
	CO2RatingBinary = find_rating(Binaries, [], fun(A, B) -> A < B end),

	OxygenRating = parse_binary(OxygenRatingBinary),
	CO2Rating = parse_binary(CO2RatingBinary),

	OxygenRating * CO2Rating.


-spec find_rating(Binaries, Prefix, Pred) -> list(0 | 1)
	when
		Binaries :: list(list(0 | 1)),
		Prefix :: list(0 | 1),
		Pred :: fun((non_neg_integer(), non_neg_integer()) -> boolean()).
find_rating([B], _, _) -> B;
find_rating(Binaries, Prefix, Pred) ->
	AmntOnes = count_ones(Binaries, length(Prefix) + 1),
	NewPrefix = Prefix ++ case Pred(AmntOnes, length(Binaries) / 2) of
		true -> [1];
		false -> [0]
	end,
	FilteredBinaries = lists:filter(fun(B) -> lists:prefix(NewPrefix, B) end, Binaries),
	find_rating(FilteredBinaries, NewPrefix, Pred).


-spec count_ones(Binaries, Index) -> non_neg_integer()
	when
		Binaries :: list(list(0 | 1)),
		Index :: non_neg_integer().
count_ones(Binaries, Index) ->
	Digits = lists:map(fun(B) -> lists:nth(Index, B) end, Binaries),
	length(lists:filter(fun(D) -> D =:= 1 end, Digits)).


-spec parse_binary(Binary) -> non_neg_integer()
	when
		Binary :: list(0 | 1).
parse_binary(Binary) ->
	lists:foldl(fun(B, Acc) -> 2 * Acc + B end, 0, Binary).


-spec get_gamma_and_epsilon(Counts, Threshold) -> Values
	when
		Counts :: list({non_neg_integer(), non_neg_integer()}),
		Threshold :: non_neg_integer(),
		Values :: list({0 | 1, 0 | 1}).
get_gamma_and_epsilon([], _) -> [];
get_gamma_and_epsilon([{GammaCount, EpsilonCount} | Rest], Threshold) ->
	GammaValue = if
		GammaCount > Threshold -> 1;
		true -> 0
	end,
	EpsilonValue = if
		EpsilonCount > Threshold -> 1;
		true -> 0
	end,
	[{GammaValue, EpsilonValue} | get_gamma_and_epsilon(Rest, Threshold)].


-spec count_ones_and_zeroes(Bin :: list(0 | 1), Acc) -> Acc
	when
		Acc :: {list(non_neg_integer()), list(non_neg_integer())}.
count_ones_and_zeroes(Bin, {GAcc, EAcc}) ->
	NewGAcc = lists:map(fun({B, G}) ->
		case B of
			1 -> G + 1;
			0 -> G
		end
	end, lists:zip(Bin, GAcc)),
	NewEAcc = lists:map(fun({B, E}) ->
		case B of
			1 -> E;
			0 -> E + 1
		end
	end, lists:zip(Bin, EAcc)),
	{NewGAcc, NewEAcc}.
