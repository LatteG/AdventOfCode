-module(day17).
-export([
	part1/0,
	part2/0
]).


-compile([nowarn_unused_function]).
-dialyzer(no_unused).


-define(V_MAX_X, 500).
-define(V_MIN_X, -?V_MAX_X).
-define(V_MAX_Y, 500).
-define(V_MIN_Y, -?V_MAX_Y).


-record(target_area, {min_x :: integer(),
					  max_x :: integer(),
					  min_y :: integer(),
					  max_y :: integer()}).
-type target_area() :: #target_area{}.
-record(coord, {x :: integer(),
				y :: integer()}).
-type coord() :: #coord{}.
-record(velocity, {x :: integer(),
				   y :: integer()}).
-type velocity() :: #velocity{}.


-spec part1() -> integer().
part1() ->
	[Line] = aoc_utils:read_file("input\\day17.txt", lines),
	TargetArea = parse_target_area(Line),
	{HighestY, Velocity} = find_highest_y(TargetArea),
	io:format("Found a velocity ~p that gets up to y=~p~n", [Velocity, HighestY]),
	HighestY.


-spec part2() -> integer().
part2() ->
	[Line] = aoc_utils:read_file("input\\day17.txt", lines),
	TargetArea = parse_target_area(Line),
	length(get_working_velocities(TargetArea)).


-spec find_highest_y(target_area()) -> {integer(), velocity()}.
find_highest_y(TargetArea) ->
	Hits = get_working_velocities(TargetArea),
	lists:foldl(fun({{hit, VY}, V}, Acc = {AY, _}) ->
		if
			VY >  AY -> {VY, V};
			VY =< AY -> Acc
		end
	end, {-1, undefined}, Hits).


-spec get_working_velocities(target_area()) -> list({{hit, integer()}, velocity()}).
get_working_velocities(TargetArea) ->
	Velocities = [#velocity{x = X, y = Y} || X <- lists:seq(?V_MIN_X, ?V_MAX_X),
											 Y <- lists:seq(?V_MIN_Y, ?V_MAX_Y)],
	Results = lists:map(fun(V) ->
		{check_velocity(TargetArea, V), V}
	end, Velocities),
	lists:filter(fun({R, _}) -> is_tuple(R) end, Results).


-spec check_velocity(target_area(), velocity()) -> {hit, integer()} | too_close | too_far.
check_velocity(TargetArea, Velocity) ->
	check_velocity(TargetArea, Velocity, #coord{x = 0, y = 0}, 0).

-spec check_velocity(target_area(), velocity(), coord(), integer()) -> {hit, integer()} | miss.
check_velocity(TargetArea, V, C, HighestY) ->
	NextC = #coord{x = C#coord.x + V#velocity.x,
				   y = C#coord.y + V#velocity.y},
	NextHighestY = max(HighestY, NextC#coord.y),
	case check_target_area(TargetArea, NextC) of
		not_reached ->
			check_velocity(TargetArea, get_next_velocity(V), NextC, NextHighestY);
		hit -> {hit, NextHighestY};
		Miss -> Miss
	end.


-spec check_target_area(target_area(), coord()) -> not_reached | hit | too_close | too_far.
check_target_area(TA, C) ->
	if
		C#coord.x >  TA#target_area.max_x 		  -> too_far;
		C#coord.y  < TA#target_area.min_y 		  -> too_close;
		C#coord.x >= TA#target_area.min_x andalso
		C#coord.y =< TA#target_area.max_y 		  -> hit;
		true 									  -> not_reached
	end.


-spec get_next_velocity(velocity()) -> velocity().
get_next_velocity(#velocity{x = X, y = Y}) ->
	NextX = if
		X =:= 0 -> 0;
		true    -> abs(X - 1) * sign_of(X)
	end,
	#velocity{x = NextX, y = Y - 1}.


-spec sign_of(integer()) -> 1 | -1.
sign_of(Num) when Num < 0 -> -1;
sign_of(_) 				  ->  1.


-spec parse_target_area(string()) -> target_area().
parse_target_area(String) ->
	[_, TargetAreaString] = string:split(String, "x="),
	BoundingCoordStrings = string:split(TargetAreaString, ", y="),
	[MinX, MaxX, MinY, MaxY] = lists:flatten(lists:map(fun(S) ->
		lists:map(fun list_to_integer/1, string:split(S, ".."))
	end, BoundingCoordStrings)),
	#target_area{min_x = MinX,
				 max_x = MaxX,
				 min_y = MinY,
				 max_y = MaxY}.
