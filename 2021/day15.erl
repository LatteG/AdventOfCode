-module(day15).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-record(coord, {x :: non_neg_integer(),
				y :: non_neg_integer()}).
-type coord() :: #coord{}.
-record(node, {c :: coord(),
			   weight :: non_neg_integer(),
			   heuristic :: non_neg_integer(),
			   path_weight:: non_neg_integer(),
			   parent :: coord() | undefined}).
-type path_node() :: #node{}.
-type weighted_map() :: list(list(non_neg_integer())).


-spec part1() -> integer().
part1() ->
	Map = aoc_utils:read_file("input\\day15.txt", digit_grid),
	{TotalWeight, _} = find_path(Map, [], []),
	% io:format("Path:~n~p~n", [Path]),
	% lists:foldl(fun(#node{c = C}, Acc) ->
	% 	#node{val = Val} = get(Map, C),
	% 	Acc + Val
	% end, 0, Path).
	TotalWeight.


-spec part2() -> integer().
part2() ->
	MapTemplate = aoc_utils:read_file("input\\day15.txt", digit_grid),
	Map = expand_map(MapTemplate, #coord{x = 5, y = 5}),
	% io:format("~p~n", [Map]),
	{TotalWeight, _} = find_path(Map, [], []),
	% io:format("Path:~n~p~n", [Path]),
	% lists:foldl(fun(#node{c = C}, Acc) ->
	% 	#node{val = Val} = get(Map, C),
	% 	Acc + Val
	% end, 0, Path).
	TotalWeight.


-spec expand_map(weighted_map(), coord()) -> weighted_map().
expand_map(Template, #coord{x = XSize, y = YSize}) ->
	Rows = lists:map(fun(Y) ->
		expand_map_row(Template, #coord{x = XSize, y = Y})
	end, lists:seq(1, YSize)),
	merge_map_rows(Rows).


-spec expand_map_row(weighted_map(), coord()) -> weighted_map().
expand_map_row(Template, #coord{x = XSize, y = Y}) ->
	Columns = lists:map(fun(X) ->
		expand_map_col(Template, #coord{x = X, y = Y})
	end, lists:seq(1, XSize)),
	merge_map_cols(Columns).


-spec expand_map_col(weighted_map(), coord()) -> weighted_map().
expand_map_col(Template, #coord{x = X, y = Y}) ->
	Increase = X + Y - 2,
	lists:map(fun(Row) ->
		lists:map(fun(Element) ->
			(Element + Increase - 1) rem 9 + 1
		end, Row)
	end, Template).


-spec merge_map_cols(list(weighted_map())) -> weighted_map().
merge_map_cols([Base | Segments]) ->
	lists:foldl(fun(M2, M1) ->
		lists:map(fun({Row1, Row2}) ->
			Row1 ++ Row2
		end, lists:zip(M1, M2))
	end, Base, Segments).


-spec merge_map_rows(list(weighted_map())) -> weighted_map().
merge_map_rows([Base | Segments]) ->
	lists:foldl(fun(M2, M1) -> M1 ++ M2 end, Base, Segments).


-spec find_path(Map :: weighted_map(), ClosedNodes, OpenNodes) -> {non_neg_integer(), list(coord())}
	when
		ClosedNodes :: list(path_node()),
		OpenNodes :: list(path_node()).
find_path(Map, [], []) ->
	ClosedNode =  #node{c = #coord{x = 1,
	 							   y = 1},
						weight = 0,
						heuristic = heuristic(Map, #coord{x = 1, y = 1}),
						path_weight = 0},
	OpenNodes = sort_nodes(get_neighbours(Map, ClosedNode)),
	find_path(Map, [ClosedNode], OpenNodes);
find_path(Map, ClosedNodes, [N | OpenNodes]) ->
	case heuristic(Map, N#node.c) of
		0 -> {N#node.path_weight + N#node.weight, backtrack(ClosedNodes, [N])};
		_ ->
			Neighbours = node_list_subtract(get_neighbours(Map, N), ClosedNodes),
			MergedNeighboursOpen = node_list_merge(OpenNodes, Neighbours),
			NewOpenNodes = sort_nodes(MergedNeighboursOpen),
			find_path(Map, [N | ClosedNodes], NewOpenNodes)
	end.


-spec backtrack(Nodes, Path) -> Path
	when
		Nodes :: list(path_node()),
		Path :: list(path_node()).
backtrack(_, Path = [#node{parent = undefined} | _]) -> Path;
backtrack(Nodes, Path = [#node{parent = PC} | _]) ->
	[ParentNode] = lists:filter(fun(#node{c = NC}) -> NC =:= PC end, Nodes),
	backtrack(Nodes, [ParentNode | Path]).


-spec node_list_subtract(NodeList, NodeList) -> NodeList
	when
		NodeList :: list(path_node()).
node_list_subtract(NL1, NL2) ->
	lists:filter(fun(#node{c = NC1}) ->
		not lists:any(fun(#node{c = NC2}) -> NC2 =:= NC1 end, NL2)
	end, NL1).


-spec node_list_merge(NodeList, NodeList) -> NodeList
	when
		NodeList :: list(path_node()).
node_list_merge(NL1, NL2) ->
	UniqueNodes = node_list_subtract(NL1, NL2) ++ node_list_subtract(NL2, NL1),
	DuplicateNL1 = node_list_subtract(NL1, UniqueNodes),
	DuplicateNL2 = node_list_subtract(NL2, UniqueNodes),
	BetterDuplicates = lists:map(fun(D1) ->
		[D2] = lists:filter(fun(D2) ->
			D2#node.c =:= D1#node.c
		end, DuplicateNL2),
		D1W = get_total_weight(D1),
		D2W = get_total_weight(D2),
		if
			D1W =< D2W -> D1;
			D1W  > D2W -> D2
		end
	end, DuplicateNL1),
	UniqueNodes ++ BetterDuplicates.


-spec heuristic(weighted_map(), coord()) -> non_neg_integer().
heuristic(Map, #coord{x = CX, y = CY}) ->
	MY = length(Map),
	MX = length(lists:nth(1, Map)),
	MX - CX + MY - CY.


-spec get_neighbours(weighted_map(), path_node()) -> list(path_node()).
get_neighbours(Map, Parent = #node{c = NC = #coord{x = X0, y = Y0}}) ->
	MaxX = length(lists:nth(1, Map)),
	MaxY = length(Map),
	Coords = [#coord{x = X, y = Y0} || X <- [X0 - 1, X0 + 1], X >= 1 andalso X =< MaxX] ++
			 [#coord{x = X0, y = Y} || Y <- [Y0 - 1, Y0 + 1], Y >= 1 andalso Y =< MaxY],
	Nodes = lists:map(fun(C) -> get(Map, C) end, Coords),
	[#node{c = N#node.c,
		   weight = N#node.weight,
		   heuristic = N#node.heuristic,
		   path_weight = Parent#node.weight + Parent#node.path_weight,
		   parent = NC} || N <- Nodes].


-spec get(weighted_map(), coord()) -> path_node().
get(Map, C = #coord{x = X, y = Y}) ->
	Weight = lists:nth(X, lists:nth(Y, Map)),
	#node{c = C,
		  weight = Weight,
		  heuristic = heuristic(Map, C)}.


-spec sort_nodes(list(path_node())) -> list(path_node()).
sort_nodes(Nodes) ->
	lists:sort(fun(Node1, Node2) ->
		WeightNode1 = get_total_weight(Node1),
		WeightNode2 = get_total_weight(Node2),
		WeightNode1 =< WeightNode2
	end, Nodes).


-spec get_total_weight(path_node()) -> non_neg_integer().
get_total_weight(#node{weight = Weight, heuristic = Heuristic, path_weight = PathWeight}) ->
	Weight + Heuristic + PathWeight.
