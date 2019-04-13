---
layout: post
title:  "System shell interfaces in Erlang"
date:   2019-04-07 08:00:00 +0300
lang: en_FI
permalink: /articles/calling-shell-from-erlang
description: >-
    There are two ways to call the system shell in Erlang.
    This is a look into their proper use and differences.
excerpt_separator: <!--more-->
categories:
    - programming
    - erlang
    - article
    - tutorial
tags:
    - qrencode
    - erlang
    - erlang port
    - shell
    - unix
    - pipe
    - inter-process communication
    - pattern matching
    - code sample
    - unicode
redirect_from: []
---
A short while ago I found myself needing to create a [QR code](https://en.wikipedia.org/wiki/QR_code){: rel="nofollow" target="_blank" __} image from data produced by an [Erlang](http://www.erlang.org){: rel="nofollow" target="_blank" __} program. Generating QR codes was not a problem, as the wonderful [`qrencode`][qrencode]{: rel="nofollow" target="_blank" __} utility by Kentaro Fukuchi makes it a breeze. In getting my bytes from Erlang to qrencode, however, I hit a few unexpected snags and learned a couple things.

<!--more-->

#### Contents
- [qrencode](#qrencode)
- [Invoking command line from Erlang](#{{ "Invoking command line from Erlang" | slugify }})
- [Erlang ports](#{{ "Erlang ports" | slugify }})
- [Ports and pipes](#{{ "Ports and pipes" | slugify }})
- [The argument vector](#{{ "The argument vector" | slugify }})
- [Unicode](#{{ "Unicode" | slugify }})
- [All files must go](#{{ "All files must go" | slugify }})
- [The End](#{{ "The End" | slugify }})
    - [Interface summary](#{{ "Interface summary" | slugify }})
    - [Files](#files)
    - [Footnotes](#footnotes)
    - [License](#license)

## qrencode

qrencode allows us to create a QR code image encoding the string "My data" a few different ways.

**Using a command line argument**
{% highlight shell-session %}$ qrencode -o qrcode.png 'My data'{% endhighlight %}

**Using a file**
{% highlight shell-session %}$ echo -n 'My data' > file; qrencode -o qrcode.png -r file; rm file{% endhighlight %}

**Using the standard input**
{% highlight shell-session %}
$ echo -n 'My data' | qrencode -o qrcode.png
$ echo -n 'My data' > file; qrencode -o qrcode.png < file; rm file
$ echo -n 'My data' > file; qrencode -o - > qrcode.png < file; rm file
{% endhighlight %}

These options, combined with the interfaces Erlang has for invoking the system shell, give us a few different approaches to try out.

## Invoking command line from Erlang

Erlang, like other programming languages, supports invoking the system shell. [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __} takes a string as an argument and executes it in a command shell. So in order to create a QR code of our preference, all we have to do is to create the right command string and execute it.

###### Example 1

{% highlight erlang %}
% Data we want in the QR code
Data = "My data",

% Prepare the command we want to execute...
Cmd  = io_lib:format("qrencode -o qrcode.png $'~s'", [Data]),

% ...and execute it
"" = Output = os:cmd(Cmd).
{% endhighlight %}

>
[`io_lib:format/2`](http://erlang.org/doc/man/io_lib.html#format-2){: rel="nofollow" target="_blank" __},
[`os:cmd/1`][erlang_os_cmd]{: rel="nofollow" target="_blank" __}
{: class="doclinks" }

Given that [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __} only returns the output of the program (both standard output and standard error are included), it seems rather better suited for cases where we call a program for its output. qrencode behaves exactly contrary to this, producing no output when it successfully creates the requested image file. The exit code of qrencode, rather than the empty output string, would be of more value to us. Alas, only the output and not the exit code is returned. If given no choice but to use [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __}, we would have to rely on our knowledge of the behavior and output patterns of qrencode to determine whether the program succeeded or not.

###### Example 2

{% highlight erlang %}
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
{% endhighlight %}

> [`os:cmd/1`][erlang_os_cmd]{: rel="nofollow" target="_blank" __}
{: class="doclinks" }

Because [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __} executes our command string in a shell, we also have to be mindful of the features and syntax of the shell when constructing the string. Had our data in the previous examples been some random string of ASCII characters, we would have had to escape any backslashes (`$\\`) and single quotes (`$'`) the data contained so as to avoid shell syntax errors and to make sure that the correct string was passed along to qrencode. Although handling such simple escape rules borders on trivial and is unlikely to cause problems, I'd still rather not do it. I already have my data in an appropriate form and would like to use it as it currently is, so mangling it for the shell just feels like a pain. My opinion is, obviously, a matter of taste and would yield in a situation with no alternatives.

Representing our data and other input in a shell-appropriate format is the major downside of shell syntax, but the shell also grants us some tools and flexibility in dealing with external commands and their outputs. Of most consequence to us are the ability to translate (non)zero exit codes into specific outputs and the ability to modify program output patterns.

###### Example 3

{% highlight erlang %}
"ok\n"    = os:cmd("(true  && echo ok) || echo error"),
"error\n" = os:cmd("(false && echo ok) || echo error").
{% endhighlight %}

> [`os:cmd/1`][erlang_os_cmd]{: rel="nofollow" target="_blank" __}
{: class="doclinks" }

###### Example 4

{% highlight erlang %}
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
{% endhighlight %}

>
[`hd/1`](http://erlang.org/doc/man/erlang.html#hd-1){: rel="nofollow" target="_blank" __},
[`lists:reverse/1`](http://erlang.org/doc/man/lists.html#reverse-1){: rel="nofollow" target="_blank" __},
[`os:cmd/2`](http://erlang.org/doc/man/os.html#cmd-2){: rel="nofollow" target="_blank" __},
[`string:lexemes/2`](http://erlang.org/doc/man/string.html#lexemes-2){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

As a small aside, I wonder how `'max_size'` limit appears to the executed program?<sup id="superscript-1">[\[1\]](#footnote-1)</sup>

###### Example 5

{% highlight erlang %}
% Or we can simply mute the noisy program
Output = os:cmd("(ls /dev &> /dev/null && echo ok) || echo error"),
case string:trim(Output, trailing) of
    "ok" -> ok;
    "error" -> error
end.
{% endhighlight %}

>
[`os:cmd/2`](http://erlang.org/doc/man/os.html#cmd-2){: rel="nofollow" target="_blank" __},
[`string:trim/2`](http://erlang.org/doc/man/string.html#trim-2){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

Note that for examples [3](#example-3), [4](#example-4) and [5](#example-5) to work, the invoked shell (whatever it may be) has to support any and all expressions you decide to use. For [example 5](#example-5) to work, the bottomless pit known as `/dev/null` also has to exist.

Ultimately examples [2](#example-2) through [5](#example-5), while they do help to get the job done, are just us making the best of our situation, ie. having to work around the behavior of [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __}. Wouldn't it be much nicer if we could somehow *just* get the exit code, since that was all that we asked for in the first place?

## Erlang ports

Enter [Erlang ports][erlang_ports]{: rel="nofollow" target="_blank" __}. Quoting the documentation:

>Ports provide the basic mechanism for communication with the external world, from Erlang's point of view. They provide a byte-oriented interface to an external program. When a port has been created, Erlang can communicate with it by sending and receiving lists of bytes, including binaries.

That sounds rather like a [pipe](https://linux.die.net/man/7/pipe){: rel="nofollow" target="_blank" __}, or maybe a [file descriptor](https://en.wikipedia.org/wiki/File_descriptor){: rel="nofollow" target="_blank" __}, doesn't it? The latter somewhat follows from the former.

Even better, a quick peek at [`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __} documentation also reveals that by using ports we can inspect program exit codes. Maybe dreams sometimes really do come true? Not only can we finally check the exit code of qrencode, we might even be able to use standard input to pass our data over to qrencode, avoiding the prettying up we'd have to do for shell. Let's start off by using ports to check the exit code.

###### Example 6

{% highlight erlang %}
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
{% endhighlight %}

>
[`io_lib:format/2`](http://erlang.org/doc/man/io_lib.html#format-2){: rel="nofollow" target="_blank" __},
[`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __}
{: class="doclinks" }

Now we get a nicer representation (for a computer anyway) of the final result of qrencode.

Yet we're still passing our data as a command line argument. As qrencode makes it possible to use a file as the contents of the created QR code, we could switch to writing our data to a file and would then only have to work with filenames while preparing our command strings. At that point we'd be more or less done with this whole exercise.

###### Example 7

{% highlight erlang %}
Data     = "My data",
DataFile = "data.txt",
Cmd      = io_lib:format("qrencode -o qrcode.png -r ~s", [DataFile]),

ok = file:write_file(DataFile, Data),
Port = open_port({spawn, Cmd}, [exit_status, stream]),
receive
    {Port, {exit_status, 0}} -> ok
end,
ok = file:delete(DataFile).
{% endhighlight %}

>
[`file:delete/1`](http://erlang.org/doc/man/file.html#delete-1){: rel="nofollow" target="_blank" __},
[`file:write_file/2`](http://erlang.org/doc/man/file.html#write_file-2){: rel="nofollow" target="_blank" __},
[`io_lib:format/2`](http://erlang.org/doc/man/io_lib.html#format-2){: rel="nofollow" target="_blank" __},
[`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __}
{: class="doclinks" }

While this works perfectly well, in my case the data I am going to encode is somewhat sensitive and I'm not too keen on writing it to disk. And, well, at this point creating a file just because I can't otherwise get a few silly bytes to qrencode rather strikes me as giving up. Files will have to make way for another approach.

## Ports and pipes

A few examples back I suggested that we might be able to use standard input to get our data to qrencode, so let's give that a whirl.

###### Example 8

{% highlight erlang %}
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
end.
{% endhighlight %}

>
[`lists:split/2`](http://erlang.org/doc/man/lists.html#split-2){: rel="nofollow" target="_blank" __},
[`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __},
[`port_close/1`][erlang_erlang_port_close]{: rel="nofollow" target="_blank" __},
[`port_command/2`](http://erlang.org/doc/man/erlang.html#port_command-2){: rel="nofollow" target="_blank" __},
[`register/2`](http://erlang.org/doc/man/erlang.html#register-2){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

Nice, right? No longer do we have to worry about shell syntax or file handling, we can just pipe anything and everything over to qrencode!

While this example does in fact correctly produce the qrcode.png file we wanted, the final `receive` expression **hangs** as it'll never match the `{Port, {exit_status, 0}}` message. Not because qrencode didn't exit cleanly (it did). Rather, it appears that while our intention was to simply signal the end of input to qrencode, the work of [`port_close/1`][erlang_erlang_port_close]{: rel="nofollow" target="_blank" __} only _starts_ there.

Mocking around with a small analogous [C companion](#files) program, my best guess is that the `'exit_status'` message does not get sent if [`port_close/1`][erlang_erlang_port_close]{: rel="nofollow" target="_blank" __} was called before the external program calls [`exit()`](https://linux.die.net/man/2/exit){: rel="nofollow" target="_blank" __}. Assuming that qrencode reads the standard input until a [`read()`](https://linux.die.net/man/2/read){: rel="nofollow" target="_blank" __} call returns 0 (indicating the end of input) and only then proceeds on to create the QR code image file, combined with the fact that our choice is between completely closing an Erlang port and not closing anything, we have no choice but to call [`port_close/1`][erlang_erlang_port_close]{: rel="nofollow" target="_blank" __} so that qrencode can detect the end of input and continue its work; yet by closing the port we lose any visibility we previously had into the status and the ultimate fate of qrencode.

Looks to me like a Catch-22. It appears that this solution, although promising in that we didn't have to mangle our data for the shell, cannot supply us with the exit code like we had hoped.

After [`port_close/1`][erlang_erlang_port_close]{: rel="nofollow" target="_blank" __} is called, not only do we only fail to receive the exit status message, we also stop receiving any standard output from the external program. Simply put, [`port_close/1`][erlang_erlang_port_close]{: rel="nofollow" target="_blank" __} really means **close**: _nothing gets in_, _nothing gets out_, and should anyone ask, _that port never even existed_. Erlang, for its part, makes sure that the atoms cannot be traced back to the port (atoms are automatically unregistered).

###### Example 9

{% highlight erlang %}
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
{% endhighlight %}

>
[`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __},
[`port_close/1`][erlang_erlang_port_close]{: rel="nofollow" target="_blank" __},
[`port_command/2`](http://erlang.org/doc/man/erlang.html#port_command-2){: rel="nofollow" target="_blank" __},
[`process_flag/2`](http://erlang.org/doc/man/erlang.html#process_flag-2){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

While we could change our approach to check for nonzero exit status messages before calling [`port_close/1`][erlang_erlang_port_close]{: rel="nofollow" target="_blank" __}, we'd be assuming that qrencode *can* and *does* do all necessary checks and verifications as it reads the input. Never mind how nice a program it is, that seems like a big ask. Even if qrencode does do all that, we still wouldn't know exactly when it has processed all our bytes *successfully* (as qrencode obviously doesn't individually acknowledge each and every byte it reads from the standard input) or when we can safely collect the produced QR code file (since we don't know when qrencode terminates or why it did).

Previous few examples detail how the interface used for closing Erlang ports makes them rather less suited for use as a pipe-equivalent form of inter-process communication (IPC). There's also another, critically important feature in which standard input/output via message passing – the way of Erlang ports – differs from how [`read()`](https://linux.die.net/man/2/read){: rel="nofollow" target="_blank" __} and [pipes](https://linux.die.net/man/7/pipe){: rel="nofollow" target="_blank" __} work, one that none of the previous examples really demonstrates. [Timer, writer and reader](#files) are here to do exactly that:

###### Pipeline

{% highlight shell-session %}
$ ./timer | ./writer | ./reader
Timer: 0 seconds
Writer: wrote 65536 additional bytes for a total of 65536
Reader: read 16384 additional bytes for a total of 16384
Reader: read 16384 additional bytes for a total of 32768
Reader: read 16384 additional bytes for a total of 49152
Reader: read 16384 additional bytes for a total of 65536
Reader: read 16384 additional bytes for a total of 81920
Reader: napping for 5 seconds before exit()
Writer: wrote 65536 additional bytes for a total of 131072
Timer: 1 second
Timer: 2 seconds
Timer: 3 seconds
Timer: 4 seconds
Reader: stopping
Writer: SIGPIPE
Writer: stopping
Timer: 5 seconds
{% endhighlight %}

Something quite different happens when we use ports in Erlang:

###### Example 10

{% highlight erlang %}
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
{% endhighlight %}

>
[`erlang:memory/0`](http://erlang.org/doc/man/erlang.html#memory-0){: rel="nofollow" target="_blank" __},
[`file:read_file_info/1`](http://erlang.org/doc/man/file.html#read_file_info-1){: rel="nofollow" target="_blank" __},
[`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __},
[`proplists:get_value/2`](http://erlang.org/doc/man/proplists.html#get_value-2){: rel="nofollow" target="_blank" __},
[`#file_info{}`](http://erlang.org/doc/man/file.html#type-file_info){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

And _**GOD FORBID**_{: title="27,816 GiB = 29866943416, 28518 messages" } should you forget to use the `'binary'` option in [`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __}.

What do these examples tell us? After reader stops pulling its weight in the [pipeline](#pipeline) example, eventually a [`write()`](https://linux.die.net/man/2/write){: rel="nofollow" target="_blank" __} call in writer will block. This behavior doesn't arise from the programs themselves; rather, it is a feature of the underlaying operating system. It provides a kind of "natural" flow control between pipeline members, allowing only a certain amount of "in-flight" data between members at any given moment.

In contrast, regardless of how many unread messages pile up in a process mailbox in [example 10](#example-10), there's never a moment when Erlang doesn't believe that, any nanosecond now, the process will just tear through every message it's got queued up, regardless of how long the queue is. The flow control provided by the operating system never triggers because the Erlang driver responsible for handling the output from the external program is constantly accepting new data, thus making sure that any limit on "in-flight" data between Erlang and the external program is only ever momentarily reached and never stalls the onslaught of bytes for long. Since we have no control over how this particular driver accepts input, _we get no flow control with Erlang ports_. Thus Erlang ports should not be used with programs which produce large to infinite amounts of output and rely on blocking for flow control.

In conclusion: it appears that while Erlang ports share one of the basic characteristics of pipes (byte-oriented communication), **in other key respects Erlang ports behave differently enough from traditional pipes so as to make using ports as pipes fraught with trouble, despair and bugs**{{ "." }} Given the fact that [Erlang interoperability documentation](http://erlang.org/doc/tutorial/c_port.html){: rel="nofollow" target="_blank" __} repeatedly describes Erlang and the external program "exchanging messages" and/or "communicating", it is not exactly shocking that the concepts don't quite map onto one another: "messages and communication", after all, implies that the participants act according to a mutually agreed upon protocol of some kind, while the lack of a protocol (save for the behavior of some basic system calls) is one of the foundational principles of the UNIX model of "one-thing-and-one-thing-well tools, connected with pipes".

### Don't use Erlang ports as pipes
{: style="text-align: center;" }

###### (Unless you know what to expect)
{: style="text-align: center;" }

## The argument vector

Three and two interfaces down and not one among them fits our case quite as we'd like?

Hold in your disappointment a moment longer, it turns out that we can use a slightly different tuple in [`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __} and then go back to supplying the data as command line arguments, all the while avoiding the hassle of data in shell syntax. Swapping `'spawn'` out for `'spawn_executable'`, we can sidestep the shell entirely and supply our command line arguments as an argument vector (argv, *AKA* `const char **argv`).

No free lunches though: while `'spawn_executable'` frees us from having to do any work to please the shell, it also frees the shell from having to do anything for us. Using `'spawn_executable'` precludes us from using any of the tricks in examples [3](#example-3), [4](#example-4) and [5](#example-5). What's more, we have to do our own legwork to find the  path of our qrencode executable, since the shell isn't around to check `PATH` for us. Fortunately Erlang provides [`os:find_executable/1,2`][erlang_os_find_executable]{: rel="nofollow" target="_blank" __} for us.

###### Example 11

{% highlight erlang %}
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
{% endhighlight %}

>
[`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __},
[`os:find_executable/1`](http://erlang.org/doc/man/os.html#find_executable-1){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

Finally! We mixed up the better elements from our previous attempts and arrived at a solution which provides the features we wanted. Nothing but smooth sailing from here on out, right?

Well, here's a slight caveat: had we used `Data = lists:seq(0, 10)`, [`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __} would have thrown a `badarg` error. While qrencode would also have refused to create a QR code from an empty string, were `Data = lists:reverse(lists:seq(0, 10))` somehow passed to it, it would have no choice but to silently produce a "corrupted" QR code<sup id="superscript-2">[\[2\]](#footnote-2)</sup>. Fortunately [`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __} would also have thrown a `badarg` in the second case, so let us thank Erlang for the sanity check. Simply put, this solution does not work when the contents of the QR code are arbitrary – possibly zero – bytes.

Also, while there might be no technical reason strictly forbidding it, I can't imagine that the argument vector was ever intended to hold 10 KiB long entries. At some point you should just give in and use a file.

###### Example 12

{% highlight erlang %}
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
{% endhighlight %}

>
[`byte_size/1`](http://erlang.org/doc/man/erlang.html#byte_size-1){: rel="nofollow" target="_blank" __},
[`lists:seq/2`](http://erlang.org/doc/man/lists.html#seq-2){: rel="nofollow" target="_blank" __},
[`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __},
[`os:find_executable/1`](http://erlang.org/doc/man/os.html#find_executable-1){: rel="nofollow" target="_blank" __},
[`process_flag/2`](http://erlang.org/doc/man/erlang.html#process_flag-2){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

## Unicode

There is an interesting detail in [example 11](#example-11). Our example data was plain ASCII text, but what would have happened if our data had contained [unicode](https://en.wikipedia.org/wiki/Unicode){: rel="nofollow" target="_blank" __} characters beyond the ASCII range – emoji, for example? The value `16#1F914 = $🤔` is rather beyond the maximum value of an unsigned byte (`16#FF`) and thus would not have been representable by an argument vector entry consisting of `char` values. Documentation for [`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __} provides the following answer:

> The name of the executable as well as the arguments specifed [sic] in cd, env, args, and arg0 are subject to Unicode filename translation if the system is running in Unicode filename mode. To avoid translation or to force, for example UTF-8, supply  the executable and/or arguments as a binary in the correct encoding. For details, see the module file(3), the function file:native_name_encoding/0 in Kernel, and the Using Unicode in Erlang User's Guide.

This is both a perfectly sane default behavior and a good reminder that while Erlang strings often map neatly onto C strings (or are transparently mapped onto C strings), they are not _inherently_ equivalent. Please also note that if your intent is to, regardless of the warning issued after [example 11](#example-11), pass arbitrary bytes along to an external program using [`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __} and _**lists**_ on platforms where `latin1 =/= file:native_name_encoding()`, every byte with a value beyond the ASCII range is going to get encoded, something that you likely don't want to happen.

Spurred on by our discovery of unicode, what would have happened if our data in [example 8](#example-8) had contained emoji (or other characters with values greater than `255 = 16#FF`)?

###### Example 13

{% highlight erlang %}
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
{% endhighlight %}

>
[`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __},
[`port_close/1`][erlang_erlang_port_close]{: rel="nofollow" target="_blank" __},
[`port_command/2`](http://erlang.org/doc/man/erlang.html#port_command-2){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

Whatever might happen if we use emoji with [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __}?

###### Example 14

{% highlight erlang linenos %}
utf8 = Encoding = file:native_name_encoding(),
true = AllowUnicode = (Encoding =/= latin1),

"🤔\n" = os:cmd("echo 🤔"),

"\n" = (catch os:cmd([$e, $c, $h, $o, $ , 0])),
{'EXIT', {function_clause, _}} = (catch os:cmd("echo \x00tail")),

Cmd1 = binary_to_list(<<"echo 🤔"/utf8>>),
[240, 159, 164, 148, $\n] = os:cmd(Cmd1),
"🤔" = unicode:characters_to_list(<<240, 159, 164, 148>>, utf8),

ok = file:make_dir("unicode_testdir"),

Cmd2 = binary_to_list(<<"touch unicode_testdir/🤔"/utf8>>),
"" = os:cmd(Cmd2),
[240, 159, 164, 148, $\n] = os:cmd("ls unicode_testdir"),
{error, enoent} = file:delete("unicode_testdir/🤔"),

"" = os:cmd("touch unicode_testdir/🤔"),
[240,159,164,148, $\n, $🤔, $\n] = os:cmd("ls unicode_testdir"),
ok = file:delete("unicode_testdir/🤔"),
Cmd3 = binary_to_list(<<"rm unicode_testdir/🤔"/utf8>>),
"" = os:cmd(Cmd3),

ok = file:del_dir("unicode_testdir").
{% endhighlight %}

>
[`binary_to_list/1`](http://erlang.org/doc/man/erlang.html#binary_to_list-1){: rel="nofollow" target="_blank" __},
[`file:del_dir/1`](http://erlang.org/doc/man/file.html#del_dir-1){: rel="nofollow" target="_blank" __},
[`file:delete/1`](http://erlang.org/doc/man/file.html#delete-1){: rel="nofollow" target="_blank" __},
[`file:make_dir/1`](http://erlang.org/doc/man/file.html#make_dir-1){: rel="nofollow" target="_blank" __},
[`file:native_name_encoding/0`](http://erlang.org/doc/man/file.html#native_name_encoding-0){: rel="nofollow" target="_blank" __},
[`os:cmd/1`](http://erlang.org/doc/man/os.html#cmd-1){: rel="nofollow" target="_blank" __},
[`unicode:characters_to_list/2`](http://erlang.org/doc/man/unicode.html#characters_to_list-2){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

[Example 14](#example-14) is rather convoluted, but the most important takeaway is clear: you _should default to using Erlang strings_, unicode and all, with [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __}. Quoting the [documentation](http://erlang.org/doc/man/os.html#type-os_command){: rel="nofollow" target="_blank" __}:

>All characters needs [sic] to be valid characters on the specific OS using file:native_name_encoding() encoding. Note that specifically null characters (integer value zero) are not allowed. However, note that not all invalid characters not necessarily will cause os:cmd/1 to fail, but may instead produce invalid results.

Lines 1-2 and 15-18 in [example 14](#example-14) further illuminate what is going on. First two lines tell us that we can use unicode in our strings and are thus not required to do anything special to represent them. Regardless, on line 15 we rebel and create `Cmd2` which contains the byte values of a UTF-8 representation of the string `"touch unicode_testdir/🤔"`. The two lists differ only in that 🤔 gets encoded as a four value/byte sequence `240, 159, 164, 148` in `Cmd2`.

As we then execute [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __} on line 16, Erlang, allowing our command string to contain unicode characters (`true = AllowUnicode`), figures that our _already encoded sequence_ `240, 159, 164, 148` is actually four separate unicode characters, each of which has to be encoded according to `utf8 = Encoding` and thus our UTF-8 encoded sequence gets encoded as UTF-8. The name of our file becomes `[195, 176, 194, 159, 194, 164, 194, 148]` instead of the correct `[240, 159, 164, 148]`.

On line 17 we list the contents of unicode_testdir directory. Erlang knows to expect UTF-8 among the retrieved bytes (`utf8 = Encoding`) and thus dutifully restores our original UTF-8 sequence, `[240, 159, 164, 148]`. Because we are at that point still a round of decoding short, 🤔 does not show up in the file list. Finally on line 18, our attempt to delete the file fails because the file `[240, 159, 164, 148]` doesn't exist. The only file that unicode_testdir contains is `[195, 176, 194, 159, 194, 164, 194, 148]`.

As a side note: although line 6 appears to contradict the documented claim "_Note that specifically null characters (integer value zero) are not allowed_", null being only allowed as the last character of a command string is in practice identical to it not actually being allowed at all, rendering the exception moot. Just don't expect the null character to show up in your commands (see [footnote 2](#footnote-2)).

Reiterating slightly: the most important takeaway from [example 14](#example-14) is that when we're dealing with [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __}, we should just use Erlang strings and everything will (likely) work out fine; any errors encountered likely have a platform specific, rather than a general, solution. However, it's good to note that unlike [`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __} (as [quoted](#unicode) at the start of this part), [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __} provides no control over text encoding and might thus be unsuited for certain uses.

## All files must go

Having spent all this time coming up with ways of avoiding the need to write our QR code content data to disk, we may as well go the distance and get rid of the image file, too.

###### Example 15

{% highlight erlang %}
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
{% endhighlight %}

>
[`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __},
[`os:find_executable/1`](http://erlang.org/doc/man/os.html#find_executable-1){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

###### Example 16

{% highlight erlang %}
Data = "My data",
Cmd = io_lib:format("qrencode -o - $'~s'", [Data]),

% -spec os:cmd(Cmd :: atom() | io_lib:chars()) -> string().
List = os:cmd(Cmd, #{max_size => 16#80000}), % 512KiB = 16#80000
Bin = list_to_binary(List).
{% endhighlight %}

>
[`io_lib:format/2`](http://erlang.org/doc/man/io_lib.html#format-2){: rel="nofollow" target="_blank" __},
[`list_to_binary/1`](http://erlang.org/doc/man/erlang.html#list_to_binary-1){: rel="nofollow" target="_blank" __},
[`os:cmd/2`](http://erlang.org/doc/man/os.html#cmd-2){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

The fact that [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __}, as per the function spec, returns a string suggests that it is probably not the best tool for large amounts of data, as a binary representation for just about any amount of text or binary data is going to be more efficient, ie. [use less memory](http://erlang.org/doc/efficiency_guide/advanced.html#memory){: rel="nofollow" target="_blank" __}. The string return type combined with the fact that all output is collected at once leads me to suggest that the use of [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __} should be restricted to cases where there is little to no output.

{% highlight erlang %}
% A couple suitable uses for os:cmd/1,2
% (assuming that a prankster hasn't replaced
% any of these with a script running "dd if=/dev/zero")
Uname = os:cmd("uname -a"),
Tmp   = os:cmd("mktemp"),
Lo    = os:cmd("ifconfig lo0").
{% endhighlight %}

>
[`os:cmd/1`](http://erlang.org/doc/man/os.html#cmd-1){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

## The End

End of the line: we have now covered most of the important details relevant to the choice of an interface to use for calling the system shell. As is often the case when computers are involved, even simple things can be complicated.

As for how I am going to create my QR codes, I know that the data I have to encode falls strictly within the ASCII range, so [`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __} with `'spawn_executable'` and an argument vector is going to fulfill my requirements just fine.

###### Interface summary

| | [`os:cmd/1,2`][erlang_os_cmd]{: rel="nofollow" target="_blank" __} | [Erlang ports][erlang_ports]{: rel="nofollow" target="_blank" __} ([`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __})
--- | --- | ---
**Execution** | Synchronous | Asynchronous
**IPC**{: title="Inter-process communication" } | No | Yes, an input pipe and an output pipe
**Input** | Input in the initial command string, possibly via shell syntax | Initial command string, shell syntax and during execution bytes can be sent using functions and/or Erlang message passing to be read by the program from certain file descriptors
**Output** | `stdout` and `stderr` are collected. Shell syntax can be used to shape the output. [`os:cmd/2`](http://erlang.org/doc/man/os.html#cmd-2){: rel="nofollow" target="_blank" __} allows a limit to be set on the number of gathered bytes. | Output written by the program to certain file descriptors is delivered via Erlang message passing as either lists or binaries.
**Return value** | Collected output is returned as a string | A port reference is returned, which is subsequently used in message passing and port-related functions
**Unicode** | The command string and the return value are both Erlang strings, unicode is handled transparently (_platform dependent_) | Erlang strings used as the command string, the executable path and/or in the argument vector are encoded transparently. Characters sent to and bytes received from the program during execution have to be explictly handled.
**Best suited for** | Executing programs with side effects and/or limited output. Since execution is synchronous, program execution time matters. | Spawning cooperative programs and then communicating with them. _Can also be used for executing programs and grabbing their output and exit code_, with certain caveats.

If you think this article gets something wrong, please do let me know. Other feelings and comments are welcome too, provided they relate to or were elicited by this piece.

You can also download the [Erlang examples](/assets/code/calling_shell_from_erlang.erl) and the [C companion](/assets/code/calling_shell_from_erlang_companion.c) and try them out for yourself. [Timer](/assets/code/calling_shell_from_erlang_timer.c), [writer](/assets/code/calling_shell_from_erlang_writer.c) and [reader](/assets/code/calling_shell_from_erlang_reader.c) were used to [illustrate](#pipeline) the difference between Erlang ports and traditional pipes. [Maxsize](/assets/code/calling_shell_from_erlang_maxsize.c) was used in [footnote 1](#footnote-1).
{: id="files" }

Or if you're not especially particular, just grab the [whole lot](/assets/code/calling_shell_from_erlang.zip).

{: style="text-align: center;"}
![QR code produced by our sample code snippets](/assets/img/calling-shell-from-erlang-qrlink.svg){: title="Our very own QR code image"}

### Once more, with feeling

#### Using a data file

###### Example 17

{% highlight erlang %}
InvokeType = cmd, % alternatively 'port' or 'port_argv'
Link       = "http://www.arv.io/articles/calling-shell-from-erlang",

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
{% endhighlight %}

>
[`apply/3`](http://erlang.org/doc/man/erlang.html#apply-3){: rel="nofollow" target="_blank" __},
[`erlang:open_port/2`](http://erlang.org/doc/man/erlang.html#open_port-2){: rel="nofollow" target="_blank" __},
[`file:delete/1`](http://erlang.org/doc/man/file.html#delete-1){: rel="nofollow" target="_blank" __},
[`file:write_file/2`](http://erlang.org/doc/man/file.html#write_file-2){: rel="nofollow" target="_blank" __},
[`filelib:is_file/1`](http://erlang.org/doc/man/file.html#read_file-1){: rel="nofollow" target="_blank" __},
[`os:cmd/1`](http://erlang.org/doc/man/os.html#cmd-1){: rel="nofollow" target="_blank" __},
[`os:find_executable/1`](http://erlang.org/doc/man/os.html#find_executable-1){: rel="nofollow" target="_blank" __},
[`string:split/3`](http://erlang.org/doc/man/string.html#split-3){: rel="nofollow" target="_blank" __},
[`string:trim/2`](http://erlang.org/doc/man/string.html#trim-2){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

#### Using command line arguments

###### Example 18

{% highlight erlang %}
InvokeType = cmd, % alternatively 'port' or 'port_argv'
Link0      = "http://www.arv.io/articles/calling-shell-from-erlang",

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
{% endhighlight %}

>
[`apply/3`](http://erlang.org/doc/man/erlang.html#apply-3){: rel="nofollow" target="_blank" __},
[`erlang:open_port/2`](http://erlang.org/doc/man/erlang.html#open_port-2){: rel="nofollow" target="_blank" __},
[`lists:foldr/3`](http://erlang.org/doc/man/lists.html#foldr-3){: rel="nofollow" target="_blank" __},
[`os:cmd/1`](http://erlang.org/doc/man/os.html#cmd-1){: rel="nofollow" target="_blank" __},
[`os:find_executable/1`](http://erlang.org/doc/man/os.html#find_executable-1){: rel="nofollow" target="_blank" __},
[`string:split/3`](http://erlang.org/doc/man/string.html#split-3){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

### Footnotes

#### Footnote 1 <a href="#superscript-1">^</a>
{: id="footnote-1" }

{% highlight erlang %}
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
{% endhighlight %}

>
[`file:delete/1`](http://erlang.org/doc/man/file.html#delete-1){: rel="nofollow" target="_blank" __},
[`file:read_file/1`](http://erlang.org/doc/man/file.html#read_file-1){: rel="nofollow" target="_blank" __},
[`io:format/2`](http://erlang.org/doc/man/io.html#format-2){: rel="nofollow" target="_blank" __},
[`length/1`](http://erlang.org/doc/man/erlang.html#length-1){: rel="nofollow" target="_blank" __},
[`os:cmd/2`](http://erlang.org/doc/man/os.html#cmd-2){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

Nothing too surprising here: after the coveted 1 KiB of output have been recovered, maxsize will eventually step into a `SIGPIPE` and terminate. Depending on the roll of the dice, the spin of the wheel, who knows, where the whims of fate may lead us, maxsize can end up writing something on the order of over 2 KiB (small writes) to over 200 KiB (large writes).

#### Footnote 2 <a href="#superscript-2">^</a>
{: id="footnote-2" }

>had we used `Data = lists:seq(0, 10)`, [`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __} would have thrown a `badarg` error. While qrencode would also have refused to create a QR code from an empty string, were `Data = lists:reverse(lists:seq(0, 10))` somehow passed to it, it would have no choice but to silently produce a "corrupted" QR code.

How so?

It all goes back to `const char **argv` and C strings. C strings have, [notoriously](https://en.wikipedia.org/wiki/Buffer_overflow){: rel="nofollow" target="_blank" __}, merely the slightest hint of structure: [K&R](https://en.wikipedia.org/wiki/The_C_Programming_Language){: rel="nofollow" target="_blank" __} introduces them as "character arrays", which is exactly what they are. C strings are character arrays which have a null character (`$\0`, `'\0'`) as their last member. Worded slightly differently, "a null character indicates the end of a C string". This means that C strings cannot represent strings which contain null characters; ie. C strings cannot represent strings like `lists:seq(0, 10)` and `lists:reverse(lists:seq(0, 10))`. The following Erlang snippet demonstrates this:

{% highlight erlang %}
Cstringify = fun (FunStr) ->
    element(1, lists:splitwith(fun (FunChar) -> FunChar =/= $\0 end, FunStr))
end,
Data1 = lists:seq(0, 10),
false = Data1 =:= ("" = Cstringify(Data1)),
Data2 = lists:reverse(Data1),
false = Data2 =:= ([10,9,8,7,6,5,4,3,2,1] = Cstringify(Data2)).
{% endhighlight %}

>
[`element/2`](http://erlang.org/doc/man/erlang.html#element-2){: rel="nofollow" target="_blank" __},
[`lists:reverse/1`](http://erlang.org/doc/man/lists.html#reverse-1){: rel="nofollow" target="_blank" __}
[`lists:seq/2`](http://erlang.org/doc/man/lists.html#seq-2){: rel="nofollow" target="_blank" __}
[`lists:splitwith/2`](http://erlang.org/doc/man/lists.html#splitwith-2){: rel="nofollow" target="_blank" __}
{: class="doclinks" }

And because the argument vector is delivered as C strings, [`open_port/2`][erlang_erlang_open_port]{: rel="nofollow" target="_blank" __} is right to throw a `badarg` error if the argument vector contains unrepresentable strings.

---

### License

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/80x15.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">System shell interfaces in Erlang</span>, and associated code samples, by <a xmlns:cc="http://creativecommons.org/ns#" href="http://www.arv.io/articles/calling-shell-from-erlang" property="cc:attributionName" rel="cc:attributionURL">Lauri Moisio</a> are licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

[qrencode]: https://fukuchi.org/works/qrencode/
[erlang_os_cmd]: http://erlang.org/doc/man/os.html#cmd-1
[erlang_ports]: http://erlang.org/doc/reference_manual/ports.html
[erlang_erlang_open_port]: http://erlang.org/doc/man/erlang.html#open_port-2
[erlang_erlang_port_close]: http://erlang.org/doc/man/erlang.html#port_close-1
[erlang_os_find_executable]: http://erlang.org/doc/man/os.html#find_executable-1
[erlang_file]: http://erlang.org/doc/man/file.html
