-module(day12).
-export([
	part1/0,
	part2/0,
	can_visit/2
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-define(start, "start").
-define(finish, "end").


-record(cave, {name :: string(),
			   size :: big | small,
			   connections :: list(string())}).
-type cave() :: #cave{}.
-type cave_system() :: #{string() => cave()}.


-spec part1() -> integer().
part1() ->
	Lines = aoc_utils:read_file("input\\day12.txt", lines),
	CaveSystem = parse_cave_system(Lines),
	Paths = find_paths(CaveSystem, [?start]),
	length(Paths).


-spec part2() -> integer().
part2() ->
	Lines = aoc_utils:read_file("input\\day12.txt", lines),
	CaveSystem = parse_cave_system(Lines),
	Paths = find_paths2(CaveSystem, [?start]),
	% io:format("Paths:~n~p~n", [Paths]),
	length(Paths).


-spec find_paths2(cave_system(), list(string())) -> list(list(string())).
find_paths2(_, Path = [?finish | _]) ->
	[lists:reverse(Path)];
find_paths2(CaveSystem, Path = [CaveName | _]) ->
	#cave{connections = PossibleConnections} = maps:get(CaveName, CaveSystem),
	Connections = lists:filter(fun(PC) ->
		C = maps:get(PC, CaveSystem),
		case C of
			#cave{name = ?start} -> false;
			#cave{size = big} -> true;
			#cave{name = N} -> can_visit(Path, N)
		end
	end, PossibleConnections),
	lists:foldl(fun(C, Acc) ->
		find_paths2(CaveSystem, [C | Path]) ++ Acc
	end, [], Connections).


-spec can_visit(list(string()), string()) -> boolean().
can_visit(Path, CaveName) ->
	SmallCaves = lists:filter(fun(N)->
		get_cave_size(N) =:= small
	end, [CaveName | Path]),
	length(SmallCaves) - sets:size(sets:from_list(SmallCaves)) =< 1.


-spec find_paths(cave_system(), list(string())) -> list(list(string())).
find_paths(_, Path = [?finish | _]) -> [lists:reverse(Path)];
find_paths(CaveSystem, Path = [CaveName | _]) ->
	#cave{connections = PossibleConnections} = maps:get(CaveName, CaveSystem),
	Connections = lists:filter(fun(PC) ->
		C = maps:get(PC, CaveSystem),
		C#cave.size =:= big orelse not lists:member(C#cave.name, Path)
	end, PossibleConnections),
	lists:foldl(fun(C, Acc) ->
		find_paths(CaveSystem, [C | Path]) ++ Acc
	end, [], Connections).


-spec parse_cave_system(list(string())) -> cave_system().
parse_cave_system(Lines) ->
	Caves = lists:flatten(lists:map(fun parse_cave/1, Lines)),
	merge_caves(Caves).


-spec merge_caves(list(cave())) -> cave_system().
merge_caves(Caves) ->
	CaveNames = sets:from_list(lists:map(fun(#cave{name = N}) -> N end, Caves)),
	sets:fold(fun(CN, Acc) ->
		Cs = lists:filter(fun(#cave{name = N}) -> N =:= CN end, Caves),
		Cave = merge_caves_connections(Cs),
		maps:put(CN, Cave, Acc)
	end, maps:new(), CaveNames).


-spec merge_caves_connections(list(cave())) -> cave().
merge_caves_connections(Caves) ->
	Connections = lists:map(fun(#cave{connections = [C]}) -> C end, Caves),
	#cave{name = Name, size = Size} = lists:nth(1, Caves),
	#cave{name = Name, size = Size, connections = Connections}.


-spec parse_cave(string()) -> list(cave()).
parse_cave(Line) ->
	[Name1, Name2] = string:split(Line, "-"),
	Size1 = get_cave_size(Name1),
	Size2 = get_cave_size(Name2),
	[#cave{name = Name1, size = Size1, connections = [Name2]},
	 #cave{name = Name2, size = Size2, connections = [Name1]}].


 -spec get_cave_size(string()) -> big | small.
get_cave_size(Name) ->
	case Name =:= string:lowercase(Name) of
		true -> small;
		false -> big
	end.
