-module(aoc_utils).
-export([
    create_day/1,
    read_file/2,
    run_day/1,
    run_days/2,
    run_days/1
]).

-type read_type() :: lines | ints | int_lines | {sorted, read_type()}.


-spec create_day(integer()) -> ok.
create_day(DayNumber) ->
    ModuleName = "day" ++ integer_to_list(DayNumber),
    ModuleContents = "-module(" ++ ModuleName ++ ").\n-export([\n\tpart1/0,\n\tpart2/0\n]).\n\n\n-spec part1() -> integer().\npart1() ->\n\t_ = aoc_utils:read_file(\"input\\\\" ++ ModuleName ++ "_test.txt\", lines),\n\t-1.\n\n\n-spec part2() -> integer().\npart2() ->\n\t_ = aoc_utils:read_file(\"input\\\\" ++ ModuleName ++ "_test.txt\", lines),\n\t-1.\n",

    file:write_file(ModuleName ++ ".erl", ModuleContents),
    file:write_file("input\\" ++ ModuleName ++ ".txt", ""),
    file:write_file("input\\" ++ ModuleName ++ "_test.txt", "").


-spec read_file(string(), read_type()) -> list(string()) | list(integer) | list(list(integer())).
read_file(FileName, {sorted, Type}) -> lists:sort(read_file(FileName, Type));
read_file(FileName, Type) ->
    {ok, FileBinary} = file:read_file(FileName),
    FileContents = unicode:characters_to_list(FileBinary),
    case Type of
        lines ->
            lists:droplast(string:split(FileContents, "\n", all));
        ints ->
            [Line] = lists:droplast(string:split(FileContents, "\n", all)),
            lists:map(fun list_to_integer/1, string:split(Line, ",", all));
        int_lines ->
            Lines = lists:droplast(string:split(FileContents, "\n", all)),
            lists:map(fun(L) ->
                          lists:map(fun list_to_integer/1, string:split(L, ",", all))
                      end, Lines)
    end.


-spec run_day(integer()) -> ok.
run_day(DayNumber) ->
    Module = list_to_atom("day" ++ integer_to_list(DayNumber)),
    io:format("Running day ~p~n", [DayNumber]),

    c:c(Module),

    BeforeP1 = erlang:timestamp(),
    Part1 = Module:part1(),
    AfterP1 = erlang:timestamp(),
    io:format("  Part 1: ~p (~ps)~n", [Part1, timer:now_diff(AfterP1, BeforeP1) / 1000000]),

    BeforeP2 = erlang:timestamp(),
    Part2 = Module:part2(),
    AfterP2 = erlang:timestamp(),
    io:format("  Part 2: ~p (~ps)~n", [Part2, timer:now_diff(AfterP2, BeforeP2) / 1000000]).


-spec run_days(integer(), integer()) -> ok.
run_days(FirstDay, LastDay) -> run_days(lists:seq(FirstDay, LastDay)).

-spec run_days(list(integer())) -> ok.
run_days(Days) -> lists:foreach(fun run_day/1, Days).
