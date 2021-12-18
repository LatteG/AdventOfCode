-module(day16).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-record(value, {version :: non_neg_integer(), % 3 bits
				type_id :: non_neg_integer(), % 3 bits
				number :: non_neg_integer()}).
-type value() :: #value{}.
-record(operator, {version :: non_neg_integer(),
				   type_id :: non_neg_integer(),
				   length_type :: bits | sub_packets,
				   length :: non_neg_integer(),
				   sub_packets :: list(packet())}).
-type operator() :: #operator{}.
-type packet() :: value() | operator().


-spec part1() -> integer().
part1() ->
	Line = aoc_utils:read_file("input\\day16.txt", lines),
	Binary = binary:decode_hex(binary:list_to_bin(Line)),
	Packets = parse_binary(Binary),
	VersionNumberSums = lists:map(fun version_number_sum/1, Packets),
	lists:sum(VersionNumberSums). % Leaving it as sum so that test input can still be run


-spec part2() -> integer().
part2() ->
	Line = aoc_utils:read_file("input\\day16.txt", lines),
	Binary = binary:decode_hex(binary:list_to_bin(Line)),
	Packets = parse_binary(Binary),
	[Values] = lists:map(fun evaluate_packet/1, Packets),
	lists:sum(Values). % Leaving it as sum so that test input can still be run


-spec evaluate_packet(packet()) -> non_neg_integer().
evaluate_packet(#value{number = Value}) -> Value;
evaluate_packet(#operator{type_id = TypeID, sub_packets = SubPackets}) ->
	Values = lists:map(fun evaluate_packet/1, SubPackets),
	case TypeID of
		0 -> lists:sum(Values);
		1 -> lists:foldl(fun(V, Acc) -> V * Acc end, 1, Values);
		2 -> lists:min(Values);
		3 -> lists:max(Values);
		5 ->
			[V1, V2] = Values,
			case V1 > V2 of
				true -> 1;
				false -> 0
			end;
		6 ->
			[V1, V2] = Values,
			case V1 < V2 of
				true -> 1;
				false -> 0
			end;
		7 ->
			[V1, V2] = Values,
			case V1 =:= V2 of
				true -> 1;
				false -> 0
			end
	end.


-spec version_number_sum(packet()) -> non_neg_integer().
version_number_sum(#value{version = Version}) -> Version;
version_number_sum(#operator{version = OperatorVersion, sub_packets = SubPackets}) ->
	SubPacketVersionSum = lists:sum(lists:map(fun version_number_sum/1, SubPackets)),
	OperatorVersion + SubPacketVersionSum.


-spec parse_binary(bitstring()) -> list(packet()).
parse_binary(<<>>) -> [];
parse_binary(<<0>>) -> [];
parse_binary(Bits) ->
	{Packet, PacketTail} = parse_packet(Bits),
	Length0 = bit_size(PacketTail) rem 8,
	<<0:Length0, Rest/bitstring>> = PacketTail,
	[Packet | parse_binary(Rest)].


-spec parse_sub_binaries(bitstring(), {bits | sub_packets, non_neg_integer()}) -> {list(packet()), bitstring()}.
parse_sub_binaries(FinalRest, {_, 0}) -> {[], FinalRest};
parse_sub_binaries(Bits, {bits, BitsLeft}) ->
	{Value, Rest} = parse_packet(Bits),
	BitsUsed = bit_size(Bits) - bit_size(Rest),
	{Values, FinalRest} = parse_sub_binaries(Rest, {bits, BitsLeft - BitsUsed}),
	{[Value | Values], FinalRest};
parse_sub_binaries(Bits, {sub_packets, PacketsLeft}) ->
	{Value, Rest} = parse_packet(Bits),
	{Values, FinalRest} = parse_sub_binaries(Rest, {sub_packets, PacketsLeft - 1}),
	{[Value | Values], FinalRest}.


-spec parse_packet(bitstring()) -> {packet(), bitstring()}.
parse_packet(<<Version:3, 4:3, Tail/bitstring>>) ->
	{Number, Rest} = get_number(Tail, []),
	Value = #value{version = Version,
				   type_id = 4,
				   number = Number},
	{Value, Rest};
parse_packet(<<Version:3, TypeID:3, 0:1, Length:15, Tail/bitstring>>) ->
	{SubPackets, Rest} = parse_sub_binaries(Tail, {bits, Length}),
	Operator = #operator{version = Version,
						 type_id = TypeID,
						 length_type = bits,
						 length = Length,
						 sub_packets = SubPackets},
	{Operator, Rest};
parse_packet(<<Version:3, TypeID:3, 1:1, Length:11, Tail/bitstring>>) ->
	{SubPackets, Rest} = parse_sub_binaries(Tail, {sub_packets, Length}),
	Operator = #operator{version = Version,
						 type_id = TypeID,
						 length_type = sub_packets,
						 length = Length,
						 sub_packets = SubPackets},
	{Operator, Rest}.


-spec get_number(bitstring(), list(bitstring())) -> {non_neg_integer(), bitstring()}.
get_number(<<0:1, BinVal:4, Rest/bitstring>>, Acc) ->
	Bits = 4 * (length(Acc) + 1),
	<<Val:Bits>> = << <<BV:4>> || BV <- lists:reverse([BinVal | Acc])>>,
	{Val, Rest};
get_number(<<1:1, BinVal:4, Rest/bitstring>>, Acc) -> get_number(Rest, [BinVal | Acc]).
