%% http://www.arv.io/assets/code/calling_shell_from_erlang.erl
%%
%% Contains examples from http://www.arv.io/articles/calling-shell-from-erlang
%% Requires at least OTP20, qrencode and basic UNIX utilities
%% (true, false, echo, ls, mktemp, touch, et al.) to work
%% You'll also need a C compiler if you want to try out
%% example_companion/0 or footnote_1/0

%% Do be careful not to mangle any important files,
%% should you decide to execute any of provided examples.

-module(calling_shell_from_erlang).

-compile(nowarn_unused_function).
-compile(nowarn_unused_vars).

-include_lib("kernel/include/file.hrl").

-export([
    example_1/0,
    example_2/0,
    example_3/0,
    example_4/0,
    example_5/0,
    example_6/0,
    example_7/0,
    example_8/0,
    example_9/0,
    % example_10/0, % Given that this an example of what NOT TO DO, don't export it
    example_11/0,
    example_12/0,
    example_13/0,
    example_14/0,
    example_15/0,
    example_16/0,
    example_17/0,
    example_17/1,
    example_18/0,
    example_18/1,
    example_companion/0,
    footnote_1/0,
    footnote_2/0
]).


example_1() ->
    % Data we want in the QR code
    Data = "My data",

    % Prepare the command we want to execute...
    Cmd  = io_lib:format("qrencode -o qrcode.png $'~s'", [Data]),

    % ...and execute it
    "" = Output = os:cmd(Cmd).


example_2() ->
    % Assuming that on success qrencode produces
    % no output and that on error it produces some
    % output, we have a simple rule based on which
    % we could write the following case expression
    Cmd = "qrencode -o qrcode.png ''", % Trying to use an empty string
    error = case os:cmd(Cmd) of
        [] -> ok;
        _ -> error
    end,

    % Or we could simply badmatch on errors
    {'EXIT', { {badmatch, _}, _} } = (catch "" = os:cmd(Cmd)),
    ok.


example_3() ->
    "ok\n"    = os:cmd("(true  && echo ok) || echo error"),
    "error\n" = os:cmd("(false && echo ok) || echo error").


example_4() ->
    % Given the line-oriented nature of the output,
    % we can also handle a noisier command.
    % In cases where the length of the output is
    % unknown but potentially large, we should use
    % os:cmd/2 to set an upper limit for the number
    % of bytes we'll accept.
    Opts = #{max_size => 4096},
    % Note, however, that if our command produced close
    % to or more than max_size of output, our final status
    % messages, also counting towards to the limit, might
    % appear only in part or not at all.
    Output = os:cmd("(ls /dev && (echo; echo ok)) || (echo; echo error)", Opts),
    case hd(lists:reverse(string:lexemes(Output, [$\r, $\n]))) of
        "ok" -> ok;
        "error" -> error;
        _ -> truncated_output
    end.


example_5() ->
    % Or we can simply mute the noisy program
    Output = os:cmd("(ls /dev &> /dev/null && echo ok) || echo error"),
    case string:trim(Output, trailing) of
        "ok" -> ok;
        "error" -> error
    end.


example_6() ->
    Data = "My data",
    Cmd  = io_lib:format("qrencode -o qrcode.png $'~s'", [Data]),

    % Instead of os:cmd/1,2, use open_port/2 to
    % execute the prepared command string.
    % Similar to os:cmd/1,2, Cmd gets executed
    % in a shell and has to respect its syntax.
    Port = open_port({spawn, Cmd}, [
        exit_status, % We want to know the exit code
        stream       % Any output from qrencode appears to us as a stream of bytes
    ]),

    % Wait until we get the clean exit code from Port
    receive
        {Port, {exit_status, 0}} -> ok
    end.


example_7() ->
    Data     = "My data",
    DataFile = "data.txt",
    Cmd      = io_lib:format("qrencode -o qrcode.png -r ~s", [DataFile]),

    ok = file:write_file(DataFile, Data),
    Port = open_port({spawn, Cmd}, [exit_status, stream]),
    receive
        {Port, {exit_status, 0}} -> ok
    end,
    ok = file:delete(DataFile).


example_8() ->
    Prev = process_flag(trap_exit, true),
    Pid = spawn_link(fun () ->
        Data = "My data",
        Cmd  = "qrencode -o qrcode.png",

        % Include a couple new options when calling open_port/2
        Port = open_port({spawn, Cmd}, [
            exit_status,
            stream,   % We send and receive a stream of bytes (like a pipe)
            use_stdio % Use standard input and output
        ]),
        true = register(qrencode, Port), % We can name the port

        {DataHead, DataTail} = lists:split(length(Data) div 2, Data),

        % Pipe our data to qrencode
        port_command(Port, DataHead),
        qrencode ! {self(), {command, DataTail}},

        % Close the port so qrencode knows not to expect more input
        port_close(qrencode),
        % Port ! {self(), close},
        % receive
        %     {Port, closed} -> ok;
        % end

        receive
            {Port, {exit_status, 0}} -> ok
        end
    end),
    receive
        {'EXIT', Pid, normal} -> ok
    after
        5000 ->
            io:format("Process taking too long to finish, killing it~n"),
            exit(Pid, kill)
    end,
    process_flag(trap_exit, Prev).


example_9() ->
    Data = "My data",
    Cmd  = "tee /dev/null",

    Prev = process_flag(trap_exit, true),

    Port = open_port({spawn, Cmd}, [exit_status, stream, use_stdio]),
    port_command(Port, Data),

    % standard output from the command gets delivered to us
    % via message passing
    receive
        {Port, {data, Data}} -> ok
    end,

    port_close(Port),

    receive
        % By trapping exits we can observe Port
        % exiting after port_close/1. Provided that no error
        % occurred in the port driver, Reason will always be
        % 'normal', giving us no additional information
        % about the fate of the external program.
        {'EXIT', Port, normal=Reason} -> ok
        %{Port, {exit_status, 0}} -> ok

        % (Port was linked to our process in open_port/2, for
        % the same reasons that gen_tcp:connect/3,4,
        % gen_udp:open/1,2, et al. link the processes that
        % invoke them.)
    end,

    process_flag(trap_exit, Prev).


example_10() ->
    % 18,75 MiB = 19660600
    19660600 = proplists:get_value(total, erlang:memory()),

    % 1,7 GiB = 1856153000
    #file_info{size=1856153000} = file:read_file_info("your name.mkv"),

    Port = open_port({spawn, "cat 'your name.mkv'"}, [
        use_stdio,
        exit_status,
        binary
    ]),
    receive
        {Port, {exit_status, 0}} -> ok
    end,

    % 1,76 GiB = 1889938600
    1889938600 = proplists:get_value(total, erlang:memory()).


example_11() ->
    Data = "My data",

    % Get the path of the executable
    Executable = os:find_executable("qrencode"),

    % Different tuple as the first argument
    Port = open_port({spawn_executable, Executable}, [
        exit_status,
        {args, [ % New option 'args', which is our argument vector
            "--output=qrcode.png",
            Data
        ]}
    ]),

    receive
        {Port, {exit_status, 0}} -> ok
    end.


example_12() ->
    % That which was never meant to be
    Prev = process_flag(trap_exit, true),
    KiB16 = << <<$a>> || _ <- lists:seq(1, 16#4000) >>,
    16384 = 16#4000 = byte_size(KiB16),
    Executable = os:find_executable("echo"),
    Port = open_port({spawn_executable, Executable}, [
        {args, ["-n", KiB16]},
        binary
    ]),
    receive
        {Port, {data, KiB16}} -> clear_the_mailbox
    end,
    receive
        {'EXIT', Port, normal} -> ok
    end,
    process_flag(trap_exit, Prev).


example_13() ->
    Data = "My data 🤔",
    Cmd  = "qrencode -o qrcode.png",

    Port = open_port({spawn, Cmd}, [exit_status, stream, use_stdio]),

    {'EXIT', {badarg, _}} = (catch port_command(Port, Data)),
    % When trying to convert our Data list into a binary,
    % the emoji causes a badarg to be thrown because the port
    % cannot make assumptions about what you meant to do by
    % trying to send the value 16#1F914, given that your intent
    % might've been any or none of the multitude of possible options:
    <<240,159,164,148>>                    = <<"🤔"/utf8>>,
    <<216,62,221,20>>                      = <<"🤔"/utf16>>,
    <<62,216,20,221>>                      = <<"🤔"/utf16-little>>,
    <<0,1,249,20>>                         = <<"🤔"/utf32>>,
    <<20,249,1,0>>                         = <<"🤔"/utf32-little>>,
    <<0,0,0,0,0,1,249,20>>                 = <<$🤔:64>>,
    <<20,249,1,0,0,0,0,0>>                 = <<$🤔:64/little>>,
    <<0,0,0,0,0,0,0,0,0,0,0,0,0,1,249,20>> = <<$🤔:128>>,
    % ...

    % When sending data to a port, and receiving data from a port,
    % character and/or value encoding and decoding have to
    % be explicitly handled (regardless of whether 'binary' option
    % was used in open_port/2).

    true = port_close(Port).


example_14() ->
    utf8 = Encoding = file:native_name_encoding(),
    true = AllowUnicode = (Encoding =/= latin1),

    "🤔\n" = os:cmd("echo 🤔"),

    "\n" = (catch os:cmd([$e, $c, $h, $o, $ , 0])),
    {'EXIT', {function_clause, _}} = (catch os:cmd("echo \x00trailing")),

    Cmd1 = binary_to_list(<<"echo 🤔"/utf8>>),
    [240, 159, 164, 148, $\n] = os:cmd(Cmd1),
    "🤔" = unicode:characters_to_list(<<240, 159, 164, 148>>, utf8),

    ok = file:make_dir("unicode_testdir"),

    Cmd2 = binary_to_list(<<"touch unicode_testdir/🤔"/utf8>>),
    "" = os:cmd(Cmd2),
    [240, 159, 164, 148, $\n] = os:cmd("ls unicode_testdir"),
    {error, enoent} = file:delete("unicode_testdir/🤔"),

    "" = os:cmd("touch unicode_testdir/🤔"),
    [240, 159, 164, 148, $\n, $🤔, $\n] = os:cmd("ls unicode_testdir"),
    ok = file:delete("unicode_testdir/🤔"),
    Cmd3 = binary_to_list(<<"rm unicode_testdir/🤔"/utf8>>),
    "" = os:cmd(Cmd3),

    ok = file:del_dir("unicode_testdir").


example_15() ->
    Data = "My data",
    Executable = os:find_executable("qrencode"),
    Port = open_port({spawn_executable, Executable}, [
        exit_status,
        binary,
        use_stdio,
        {args, ["--output=-", Data]} % qrencode will write the image to stdout
    ]),
    example_15(Port, <<>>).

example_15(Port, Buf) ->
    receive
        {Port, {data, Data}} -> example_15(Port, <<Buf/binary, Data/binary>>);
        {Port, {exit_status, 0}} -> {ok, Buf}
    end.


example_16() ->
    Data = "My data",
    Cmd = io_lib:format("qrencode -o - $'~s'", [Data]),

    % -spec os:cmd(Cmd :: atom() | io_lib:chars()) -> string().
    List = os:cmd(Cmd, #{max_size => 16#80000}), % 512KiB = 16#80000
    Bin = list_to_binary(List).


example_17() ->
    example_17(cmd). % alternatively 'port' or 'port_argv'

example_17(InvokeType) ->
    Link = "http://www.arv.io/articles/calling-shell-from-erlang",

    % Create and write the data file
    % Use mktemp so that the file is atomically created
    % with 0600 permissions
    DataFile = string:trim(os:cmd("mktemp"), trailing),
    true = filelib:is_file(DataFile),
    ok = file:write_file(DataFile, Link),

    Cmd = "qrencode -t SVG -v 10 -o qrlink.svg -r " ++ DataFile,

    try
        case InvokeType of
            cmd ->
                "" = os:cmd(Cmd), % badmatch on error
                ok;
            _ ->
                BaseOpts = [exit_status, stream, use_stdio, stderr_to_stdout],
                Args = case InvokeType of
                    port -> [ {spawn, Cmd}, BaseOpts ];
                    port_argv ->
                        [ExecutableName|Argv] = string:split(Cmd, " ", all),
                        ExecutablePath = os:find_executable(ExecutableName),
                        [
                            {spawn_executable, ExecutablePath},
                            [{args, Argv}|BaseOpts]
                        ]
                end,
                Port = apply(erlang, open_port, Args),
                case
                    % Match only messages related to Port
                    receive {Port, _}=Msg -> Msg end
                of
                    {Port, {exit_status, 0}} -> ok % But crash on error
                end
        end
    after
        % Whatever the result, let's clean up after ourselves
        ok = file:delete(DataFile)
    end.


example_18() ->
    example_18(cmd). % alternatively 'port' or 'port_argv'

example_18(InvokeType) ->
    Link0 = "http://www.arv.io/articles/calling-shell-from-erlang",

    Cmd = "qrencode -t SVG -v 10 -o qrlink.svg",

    Link = fun () ->
        Escape = fun
            ($',  FunAcc) -> "\\'"  ++ FunAcc;
            ($\\, FunAcc) -> "\\\\" ++ FunAcc;
            (C,   FunAcc) -> [C|FunAcc]
        end,
        " $'" ++ lists:foldr(Escape, "'", Link0)
    end,

    case InvokeType of
        cmd ->
            "" = os:cmd(lists:append(Cmd, Link())), % badmatch on error
            ok;
        _ ->
            BaseOpts = [exit_status, stream, use_stdio, stderr_to_stdout],
            Args = case InvokeType of
                port -> [ {spawn, lists:append(Cmd, Link())}, BaseOpts ];
                port_argv ->
                    [ExecutableName|Argv] = string:split(Cmd, " ", all),
                    ExecutablePath = os:find_executable(ExecutableName),
                    [
                        {spawn_executable, ExecutablePath},
                        [{args, lists:append(Argv, [Link0])}|BaseOpts]
                    ]
            end,
            Port = apply(erlang, open_port, Args),
            case
                % Match only messages related to Port
                receive {Port, _}=Msg -> Msg end
            of
                {Port, {exit_status, 0}} -> ok % But crash on error
            end
    end.


% Requires that the accompanying C program
% http://www.arv.io/assets/code/calling_shell_from_erlang_companion.c
% resides in the Erlang working directory and is called companion
example_companion() ->
    Args = [
        <<"Hello">>,
        "World",
        "🤔"
    ],
    Port = open_port({spawn_executable, "./companion"}, [
        stream,
        use_stdio,
        exit_status,
        binary,
        {args, Args} % While we're at it, demonstrate argv
    ]),
    {ok, Buf} = receive
        {Port, {data, <<"Meowdy-do, companion ready\n", Tail/binary>>}} -> {ok, Tail}
    end,
    ok = example_companion_argv(Port, Args, Buf),
    true = port_command(Port, "ping"),
    receive
        {Port, {data, <<"Loop\n">>}} -> ok
    end,
    % Introduce a chance we'll see the last <<"Loop\n">> message
    Delay = (rand:uniform(3) - 1),
    true = port_command(Port, "ping"),
    receive after Delay -> ok end,
    % Regardless, we'll never receive the goodbye or the exit status
    % as it is not possible for the C program to write that before
    % we call port_close/1. And after port_close/1... Well, the
    % connection is severed
    true = port_close(Port),
    io:format("Delay was ~B~n", [Delay]),
    % Since companion inherits standard error from Erlang and since
    % with the above options Erlang doesn't capture or redirect it
    % anywhere, couple things will happen before example_companion/0
    % finally returns an 'ok' atom:
    % - We might see an error message that the companion writes to
    %   standard error. The error message can appear before or after
    %   Erlang prints out the randomly selected Delay value.
    % - We'll see a goodbye line that the companion writes to
    %   standard error. The goodbye line can appear before or after
    %   Erlang prints out the randomly selected Delay value.
    example_companion_await().

example_companion_argv(_, [], <<>>) ->
    ok;
example_companion_argv(Port, [Arg0|Rest], Buf) ->
    {Arg, Len} = case is_binary(Arg0) of
        true -> {Arg0, byte_size(Arg0)};
        false ->
            TmpArg = unicode:characters_to_binary(Arg0, unicode, file:native_name_encoding()),
            {TmpArg, byte_size(TmpArg)}
    end,
    Part = {0, Len},
    case binary:match(Buf, Arg, [{scope, Part}]) of
        Part ->
            <<_:Len/binary, Tail/binary>> = Buf,
            example_companion_argv(Port, Rest, Tail);
        nomatch ->
            receive
                {Port, {data, Data}} -> example_companion_argv(Port, [Arg|Rest], <<Buf/binary, Data/binary>>)
            end
    end.

example_companion_await() ->
    receive
        Msg ->
            io:format("Received a message ~p~n", [Msg]),
            example_companion_await()
    after
        200 -> ok
    end.



% Requires that the accompanying C program
% http://www.arv.io/assets/code/calling_shell_from_erlang_maxsize.c
% resides in the Erlang working directory and is called maxsize
footnote_1() ->
    Output = os:cmd("./maxsize", #{max_size => 1024}),
    1024 = length(Output),
    {ok, Log1} = file:read_file(".death note.txt"),
    ok = footnote_1_check_log(Log1, 0),

    Output = os:cmd("./maxsize large-write", #{max_size => 1024}),
    1024 = length(Output),
    {ok, Log2} = file:read_file(".death note.txt"),
    ok = footnote_1_check_log(Log2, 0),

    OutputSingle = os:cmd("./maxsize single", #{max_size => 1024}),
    256 = length(OutputSingle),
    {ok, Log3} = file:read_file(".death note.txt"),
    ok = footnote_1_check_log(Log3, 0),

    % While this makes rather little sense, it works
    [] = os:cmd("./maxsize", #{max_size => 0}),
    {ok, Log4} = file:read_file(".death note.txt"),
    ok = footnote_1_check_log(Log4, 0),
    ok = file:delete(".death note.txt").

footnote_1_check_log(<<"Wrote 65536 additional bytes\n", Rest/binary>>, Count) ->
    footnote_1_check_log(Rest, Count + 256); % 256 ^ 2 = 65536
footnote_1_check_log(<<"Wrote 256 additional bytes\n", Rest/binary>>, Count) ->
    footnote_1_check_log(Rest, Count + 1);
footnote_1_check_log(<<"SIGPIPE\n", "Stopping\n">>, Count) ->
    io:format("maxsize wrote ~B bytes~n", [Count * 256]),
    ok;
footnote_1_check_log(<<"Stopping\n">>, Count) ->
    io:format("maxsize wrote ~B bytes and terminated without a SIGPIPE~n", [Count * 256]),
    ok.


footnote_2() ->
    Cstringify = fun (FunStr) ->
        element(1, lists:splitwith(fun (FunChar) -> FunChar =/= $\0 end, FunStr))
    end,
    Data1 = lists:seq(0, 10),
    false = Data1 =:= ("" = Cstringify(Data1)),
    Data2 = lists:reverse(Data1),
    false = Data2 =:= ([10,9,8,7,6,5,4,3,2,1] = Cstringify(Data2)).
