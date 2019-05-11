---
layout: post
title:  Raw keyboard input in Erlang on Linux
date:   2019-05-06 15:30:00 +0300
lang: en_FI
permalink: /articles/raw-keyboard-input-erlang-linux
description: >-
    Small exploration of how an Erlang programmer could go about
    acquiring raw keyboard input on Linux.
excerpt_separator: <!--more-->
categories:
    - erlang
tags:
    - erlang
redirect_from: []
---
No doubt you have come across one of the widely available USB [barcode](https://www.verkkokauppa.com/fi/search?query=viivakoodi){: rel="nofollow" target="_blank" __} or [RFID](https://www.amazon.de/s?k=rfid&i=computers){: rel="nofollow" target="_blank" __} readers. They vary in design and capabilities but mostly act the same. The best part is, you can plug one into your computer and use it without having to deal with drivers, configuration and other hassles.

It turns out that while they certainly are handy if you are trying to use them in a browser window, a text file or maybe a spreadsheet, they tend to be less than straightforward to use with smaller, GUI-less programs, especially when the program is intended to run as a daemon or otherwise without a terminal.

I wanted to capture input from such a reader in an [Erlang](http://erlang.org){: rel="nofollow" target="_blank" __} program. This led me to take a small tour of how Linux handles keyboard input, how one would go about interpreting said input and how one would interface that with Erlang.

<!--more-->

#### Contents
- [USB barcode (and other) readers](#{{ "USB barcode (and other) readers" | slugify }})
- [Input devices on Linux](#{{ "Input devices on Linux" | slugify }})
- [struct input_event and keycodes](#struct-input_event-and-keycodes)
- [The keyboard illusion](#{{ "The keyboard illusion" | slugify }})
- [Sample reader input](#{{ "Sample reader input" | slugify }})
- [Isn't everything a file in UNIX](#isnt-everything-a-file-in-unix)
- [The Erlang driver](#{{ "The Erlang driver" | slugify }})
    - [Behavior](#{{ "Behavior" | slugify }})
    - [Driver initialization](#{{ "Driver initialization" | slugify }})
    - [Returning the input](#{{ "Returning the input" | slugify }})
    - [Sample driver input](#{{ "Sample driver input" | slugify }})
    - [(Re-)Connected devices](#{{ "(Re-)Connected devices" | slugify }})
    - [Captured, not gone](#{{ "Captured, not gone" | slugify }})
- [The dark side of the barcode reader](#{{ "The dark side of the barcode reader" | slugify }})
- [The End](#{{ "The End" | slugify }})
    - [Files](#files)
    - [Footnotes](#footnotes)
    - [License](#license)

## USB barcode (and other) readers

These readers differ in capabilities (support for 1D and 2D barcode variants varies, as does the supported RFID frequency), but they all have one thing in common: they show up as USB input devices and when a barcode – or an RFID tag – is read, the device simulates the correct keyboard button presses to produce the string encoded by the barcode (or tag), followed by enter key.

Although this solution could be perceived as little hackish, in practice it is a pragmatic way to bring these machine-readable inputs to a wider audience than has previously been possible. And because these devices act according to a simple, predictable script, one could, for example, fairly easily create a small form application with [React](https://reactjs.org){: rel="nofollow" target="_blank" __}, or maybe even shell scripting, to speed up data input in some specific case. Thus by removing the requirement for specific drivers and the necessity to read input via some interface, instead making these readers seem like particularly furious human typists, they have the potential to make data input more convenient and less error prone in a broad range of different situations.

Just please don't decide to encode arbitrary commands which are later to be executed when read into a shell

{% highlight shell %}
rm -rf ~/*

💣(){ 💣|💣& };💣
{% endhighlight %}

Exactly because these devices act as though they were just plain-ordinary keyboards in the hands of caffeine-addled typists and because they are a pragmatic, but not necessarily a general/correct, solution to bring machine-readable input to the environment in which most computer users reside, extracting them from this assumed environment can introduce problems. Even altering the environment slightly can be trouble.

Case in point, using a barcode reader bought in Finland with a UK bought computer: `-`, `_`, `/`, `?`, `<` and `>` characters, among others, are produced by different keys on the Finnish keyboard than on the UK keyboard. The barcode reader, assuming one or the other keyboard, would in certain combinations produce the incorrect input. There's obviously less of a chance for problems when dealing with numbers-only codes (like [EAN/UPC](https://www.gs1.org/standards/barcodes/ean-upc){: rel="nofollow" target="_blank" __}) but one should be careful, nonetheless. Verifying that certain keys are produced the same on _every_ keyboard in the world is hardly straightforward (or _antifragile_).

Also, have you by chance paid attention to what happens when you press Caps lock on a Linux computer with multiple keyboards attached? What happens is that the Caps lock led gets toggled on all of them. Ie. the Caps lock state is global. Thus a USB barcode reader, assuming that Caps lock is not on, would in that situation produce the incorrect input. I'm sure how Caps lock is handled when multiple keyboards are attached varies by operating system (and maybe even by Linux distribution) and some codes may well be dealt with in a case-insensitive manner, but just keep in mind that a number of assumptions go into the correct behavior of these USB readers.

Removing the windowed GUI and the terminal environments completely (by capturing the raw input from these devices) frees us from some of these concerns and introduces new ones. But first we'll need a way to get our hands on that raw input.

## Input devices on Linux

You might have heard somewhere that "Everything's a file in UNIX". Linux, a (spiritual) descendant of the UNIX family of operating systems, adheres to this principle and exposes a range of devices as files, traditionally in the `/dev` directory. Input devices, among which keyboards and mice are obviously counted, are no exception. According to the [documentation](https://github.com/torvalds/linux/blob/master/Documentation/input/input.rst#detailed-description){: rel="nofollow" target="_blank" __}, device drivers feed events into the kernels input subsystem, which further exposes these events as (device) files in the `/dev/input` directory. Depending on the configuration of your machine, the `/dev/input` directory might contain a number of files.

{% highlight shell-session %}
$ ls /dev/input
by-id  by-path  event0  event1  mice
{% endhighlight %}

These names are not much use to us if we're trying to determine what device is what. We have a few options when trying to suss out the exact identities of these devices.

{% highlight shell-session %}
$ cat /sys/class/input/event0/device/name
{% endhighlight %}

This solution might yield instructive results like `Barcode Scanner Barcode Scanner` (an actual barcode scanner) or `Barcode Reader` (an RFID reader).

In practice you tend to be better off using [`udevadm`](https://linux.die.net/man/8/udevadm){: rel="nofollow" target="_blank" __} and looking for vendor id (`ID_VENDOR_ID`) and model id (`ID_MODEL_ID`). These values, although not particularly human-friendly, are likely a good bet for identifying the device when you know what you are looking for. As for how you figure out _what to look for_, my best suggestion is trial and error combined with a bit of legwork: capture input from a device and then try to produce input on a physical device. If you get something, it's probably a match.

{% highlight shell-session %}
$ udevadm -q all /dev/input/event0
{% endhighlight %}

Although the devices found in the `/dev/input` directory produce the same format of output, we can't just do something like `cat /dev/input/event0`, as it'll just result in a garbled shell. To understand the output format of these devices, we need to go back to the [documentation](https://github.com/torvalds/linux/blob/master/Documentation/input/input.rst#event-interface){: rel="nofollow" target="_blank" __} (and checking the relevant header files [`linux/input.h`](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input.h){: rel="nofollow" target="_blank" __} and [`linux/input-event-codes.h`](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h){: rel="nofollow" target="_blank" __} doesn't hurt either).

From the documentation we learn that on Linux it is preferred that input devices implement an interface known as "evdev" and that devices adhering to the evdev interface produce a stream of `struct input_event` values as their output. Thus writing our program we can include the necessary header files (`linux/input.h`), [`open()`](https://linux.die.net/man/2/open){: rel="nofollow" target="_blank" __} a device file and then enjoy the `struct input_event` stream we can [`read()`](https://linux.die.net/man/2/read){: rel="nofollow" target="_blank" __} from it.

###### Example 1

{% highlight c %}
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <linux/input.h>

int main(int argc, const char **argv)
{
    if (!(++argv, --argc))
        return -1; // No device file specified

    int fd = open(*argv, O_RDONLY);
    if (fd < 0) {
        perror("Failed to open() device");
        return -1;
    }

    struct input_event buf[10];
    int rd;
    while ((rd = read(fd, buf, sizeof(buf))) > 0) {
        // Do something with input
    }

    return 0;
}
{% endhighlight %}
{: ****}

Obviously this will only work if we execute the program as a user with sufficient privileges, as it hardly makes sense to expose the input from a keyboard to any and all observers (I hear such programs are sometimes called keyloggers). This is why event device files typically have something like `0660` permissions with `root` and some restricted group as owners.

**...But that is C! What about Erlang?** To get the `struct input_event` values which Linux offers as an interface to userspace, we have to use C (or some other suitably compatible language). Although our ambition is to ultimately use Erlang, we have to involve C somehow, either as NIFs to translate the otherwise acquired bytes into terms representing the input events, or as a means of acquiring the bytes and translating them before they are somehow delivered to Erlang. We'll return to this in a moment.

## struct input_event and keycodes

Consulting the [documentation](https://github.com/torvalds/linux/blob/master/Documentation/input/input.rst#event-interface){: rel="nofollow" target="_blank" __}, we can start planning how we'll make use of the contents of the `struct input_event` values output by our device.

###### struct input_event

{% highlight c %}
struct input_event {
        struct timeval time;
        unsigned short type;
        unsigned short code;
        unsigned int value;
};
{% endhighlight %}

We can find explanations for type, code and value fields in [documentation](https://github.com/torvalds/linux/blob/master/Documentation/input/event-codes.rst){: rel="nofollow" target="_blank" __} and the relevant [header file](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h){: rel="nofollow" target="_blank" __}.

If we were to somehow receive each of these input events as an Erlang message<sup id="superscript-1">[\[1\]](#footnote-1)</sup>, a single press of the left control key would look something like the following:

###### Example 2

{% highlight erlang %}
-type input_event() :: {
    'input',
    Port      :: port(),
    Timestamp :: {Sec :: non_neg_integer(), MicroSec :: non_neg_integer()},
    Type      :: atom(),
    Key       :: non_neg_integer(),
    Value     :: non_neg_integer()
}.

flush(). % We expect to receive a bunch of input_event() terms
Shell got {input, #Port<0.9>, {1537055034,859851}, msc,  4, 458976}
Shell got {input, #Port<0.9>, {1537055034,859851}, key, 29, 1}
Shell got {input, #Port<0.9>, {1537055034,859851}, syn,  0, 0}
Shell got {input, #Port<0.9>, {1537055034,947861}, msc,  4, 458976}
Shell got {input, #Port<0.9>, {1537055034,947861}, key, 29, 0}
Shell got {input, #Port<0.9>, {1537055034,947861}, syn,  0, 0}
{% endhighlight %}

We can see the `'key'` events where the left control (`Key = 29`) is depressed (`Value = 1`) and released (`Value = 0`), surrounded by other events. `'syn'` events delimit different physical events (key down, key up) and the meaning of the `'msc'` event is rather a mystery to me.

So how do we know what key a code represents? We can find the definitions in the [`linux/input-event-codes.h`](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h){: rel="nofollow" target="_blank" __} header file.

{% highlight c %}
#define KEY_LEFTCTRL		29
{% endhighlight %}

And funnily enough, [`KeyboardEvent`](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent){: rel="nofollow" target="_blank" __} events in the browser similarly allow us to detect keycodes. You can type a character into the input field below to discover its keycode.

###### KeyboardEvent example

<div id="keycodeDemo" style="text-align: center; margin-top: 15px; margin-bottom: 15px;">
    <h4 id="keyCodeDisplay">Keycode</h4>
    <input id="keyCodeInput" type="text" style="text-align: center; width: 50px; height: 30px; border-radius: 3px; border-color: rgba(16, 16, 16, 0.33);">
</div>

<script type="text/javascript">
(function(global){
    var input   = document.getElementById('keyCodeInput'),
        display = document.getElementById('keyCodeDisplay');

    function keyDown(e) {
        e.preventDefault();
        input.value = e.key;
        display.innerHTML = "" + e.code;
    }

    if (input !== null)
        input.addEventListener('keydown', keyDown);
    else
        console.log("Could not get input element");
}(window));
</script>

`Digit1` is not exactly the same as `KEY_1`, nor is `ControlLeft` quite `KEY_LEFTCTRL` but it's certainly close enough to see the connection. And while this doesn't give us any sort numerical value for a key, I would argue that `ControlLeft` and `KEY_LEFTCTRL` are a much nicer representations for the key than the magic number `29`. Not to mention that although these concern the same event (a key press), they are rather on different levels of abstraction and involve different standards. As long as browsers agree on `ControlLeft` and Linux drivers agree on `KEY_LEFTCTRL`, sanity reigns.

On that note, let's spend a moment considering how nice it is to have an operating system abstracting devices into a standard interface. Because Linux exposes input devices with the same evdev interface, we don't have to worry about how the device is connected (PS/2, USB, Bluetooth, some internal connection on a laptop) nor do we have to worry about the specifics of the device. Linux abstracts away the connection and also the raw scancodes the device produces, instead giving us the `struct input_event` values with standard event types and keycodes. Without such a convenience, our dream of accessing the raw keyboard events would quickly wither and die, as the complexity would quickly grow to insurmountable levels.

## The keyboard illusion

Although at first it doesn't seem too far out to imagine a keyboard as a stream of input events, it also doesn't take long to encounter the following conundrum: shift _always_ produces an event with keycode `KEY_LEFTSHIFT`, `A` key _always_ produces an event with keycode `KEY_A`, regardless of whether a shift key was depressed when `A` was pressed or not. `CAPSLOCK` key produces an event with keycode `KEY_CAPSLOCK` and the keycode for `A` remains `KEY_A`, regardless of whether an led on the keyboard is lit up or not. Inspecting the [header file](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h){: rel="nofollow" target="_blank" __}, one cannot find definitions like KEY_a, KEY_SHIFT_A or anything to that effect.

It turns out that at this level, the keys we normally perceive as modifiers (shift, alt, alt gr, control, caps lock, "windows key", etc.) are just keys among fellow keys. It is up to the consumers of these input events to produce the special behavior that we expect of modifier keys.

Another detail of interest: peeking into the Linux header files, you can find codes like `KEY_A`, `KEY_LEFTSHIFT` but nothing like a KEY_Ö. Yet an `Ö` key sits on my Finnish keyboard. Hitting it produces an event with keycode `KEY_SEMICOLON`, but on my keyboard semicolon is produced by `Shift+comma`. You can [go back](#{{ "KeyboardEvent example" | slugify }}) and check out how the weirder keys on your keyboard register. What is actually going on?

Welcome to the world of keyboard layouts. Although in Linux the standard keycodes underlay keyboard hardware, the consumers of keyboard events transparently map these codes to different characters, not unlike how the effects of modifier keys get conjured up. Thus `KEY_SEMICOLON` becomes `Ö` on the Finnish keyboard and the actual semicolon gets relocated.

So as it turns out, some slight of hand goes into making a keyboard work the way we perceive it working. And because we are intentionally bypassing the normal consumer of these events (both the terminal and the GUI), it will be up to us to produce this behavior.

> Aside: as someone who made the decision/mistake to learn Dvorak instead of the standard Qwerty layout as a wee lad, the illusionary nature of keyboard layouts has always been particularly visceral to me: although it says `Q` on the button, I know it'll produce a `'` instead, `W` is comma, `E` is dot, `R` is `P`, `T` is `Y`, `Y` is `F` and so on. I imagine the case is somewhat similar for people who have to regularly communicate in different languages that don't necessarily even share a common script. For example, one might be required to write in Finnish и по-русски (and in Russian).

## Sample reader input

Here's a small sample (again in the form of [`input_event()`](#example-2) terms) of a barcode reader producing the events for a barcode encoding the string `CODE128A`. `EV_MSC` and `EV_SYN` events have been pruned out.

###### Example 3

{% highlight erlang %}
Shell got {input, #Port<0.11>, {1537378429,768308}, key, 42, 1} % KEY_LEFTSHIFT
Shell got {input, #Port<0.11>, {1537378429,768308}, key, 46, 1} % KEY_C
Shell got {input, #Port<0.11>, {1537378429,776281}, key, 42, 0} % KEY_LEFTSHIFT
Shell got {input, #Port<0.11>, {1537378429,776281}, key, 46, 0} % KEY_C
Shell got {input, #Port<0.11>, {1537378429,784289}, key, 42, 1} % KEY_LEFTSHIFT
Shell got {input, #Port<0.11>, {1537378429,784289}, key, 24, 1} % KEY_O
Shell got {input, #Port<0.11>, {1537378429,792268}, key, 42, 0} % KEY_LEFTSHIFT
Shell got {input, #Port<0.11>, {1537378429,792268}, key, 24, 0} % KEY_O
Shell got {input, #Port<0.11>, {1537378429,800290}, key, 42, 1} % KEY_LEFTSHIFT
Shell got {input, #Port<0.11>, {1537378429,800290}, key, 32, 1} % KEY_D
Shell got {input, #Port<0.11>, {1537378429,808273}, key, 42, 0} % KEY_LEFTSHIFT
Shell got {input, #Port<0.11>, {1537378429,808273}, key, 32, 0} % KEY_D
Shell got {input, #Port<0.11>, {1537378429,816283}, key, 42, 1} % KEY_LEFTSHIFT
Shell got {input, #Port<0.11>, {1537378429,816283}, key, 18, 1} % KEY_E
Shell got {input, #Port<0.11>, {1537378429,824276}, key, 42, 0} % KEY_LEFTSHIFT
Shell got {input, #Port<0.11>, {1537378429,824276}, key, 18, 0} % KEY_E
Shell got {input, #Port<0.11>, {1537378429,832285}, key,  2, 1} % KEY_1
Shell got {input, #Port<0.11>, {1537378429,840266}, key,  2, 0} % KEY_1
Shell got {input, #Port<0.11>, {1537378429,848278}, key,  3, 1} % KEY_2
Shell got {input, #Port<0.11>, {1537378429,856272}, key,  3, 0} % KEY_2
Shell got {input, #Port<0.11>, {1537378429,864275}, key,  9, 1} % KEY_8
Shell got {input, #Port<0.11>, {1537378429,872268}, key,  9, 0} % KEY_8
Shell got {input, #Port<0.11>, {1537378429,880296}, key, 42, 1} % KEY_LEFTSHIFT
Shell got {input, #Port<0.11>, {1537378429,880296}, key, 30, 1} % KEY_A
Shell got {input, #Port<0.11>, {1537378429,888272}, key, 42, 0} % KEY_LEFTSHIFT
Shell got {input, #Port<0.11>, {1537378429,888272}, key, 30, 0} % KEY_A
Shell got {input, #Port<0.11>, {1537378429,896285}, key, 28, 1} % KEY_ENTER
Shell got {input, #Port<0.11>, {1537378429,904263}, key, 28, 0} % KEY_ENTER
{% endhighlight %}

It looks a little silly, seeing the gremlins toggling shift on and off, but in the end it gets the job done. As I said previously, hackish and pragmatic.

## Isn't everything a file in UNIX

Didn't we previously say that "everything's a file in UNIX"? Can't we use the [`file`](http://erlang.org/doc/man/file.html){: rel="nofollow" target="_blank" __} module in Erlang to read input from our keyboard? Then we could write some [NIFs](http://erlang.org/doc/man/erl_nif.html){: rel="nofollow" target="_blank" __} which would translate the read bytes into proper Erlang terms.

While we can actually open an input device file with `file:open/2`, getting the input this way does not actually work. `file:read/2` seems to just hang while `file:read/2` on a file opened in `'raw'` mode produces an `{'error', 'einval'}` error.

It turns out "Everything is a file in UNIX" is not actually _literally true_, but instead articulates a wonderful interface decision the UNIX designers made back around the epoch. `file` module, which is meant for handling normal files (opposed to device files, such as our reader/keyboard), is not going to work in our case.

## The Erlang driver

Although you could go about reading input from a device any number of different ways, I elected to write a small [Erlang driver](http://erlang.org/doc/man/erl_driver.html){: rel="nofollow" target="_blank" __} to do it. In part as practice, in part because a driver fits the model of how a reader is going to interface with our program: we wait until the sporadic inputs constitute the whole we're interested in and then do something with the buffered whole.

You can download the [c code](/assets/code/erlang_evdev_driver.c) for the driver or the [source code](/assets/code/erlang_evdev_driver.zip) for the whole driver application.

### Behavior

Our driver is going to open a single device for reading when the driver is started and then read and forward events from that device to us until the driver is terminated or it encounters an error. We'll supply the path of the device when we're spawning the driver.

{% highlight erlang %}
% Our driver is called 'erlang_evdev_driver'
% The device we're interested in is '/dev/input/event0'
Port = open_port({spawn_driver, "erlang_evdev_driver /dev/input/event0"}, []).
{% endhighlight %}

The driver will not forward all the events it reads from a device (like is done in [example 2](#example-2)), but instead will only forward `EV_KEY` (keyboard key) events to us. It also won't forward any key event types other key down (`Value = 1`). It won't forward any events concerning modifier keys: we'll instead maintain modifier key state as a bit field in the driver. The current state of modifiers is represented as a list of atoms which is included in each message and re-included whenever modifier states change. Keycodes for each event are forwarded as integers. Enter is represented by atom `'end'`.

{% highlight erlang %}
-type evdev_drv_modifier() ::
      'shift'
    | 'capslock'
    | 'numlock'
    | 'control'
    | 'alt'
    | 'alt_gr'
    | 'meta'.

-type evdev_drv_event() :: {
    'input',
    port(),
    [[evdev_drv_modifier()] | 0..16#FFFF | 'end']
}.
{% endhighlight %}

We can then handle input from the reader fairly easily: we can choose a keymap based on the modifiers and then accrue input until an `'end'` atom is encountered.

{% highlight erlang %}
handle_input(['end'|Rest], Acc, Fn, Port) ->
    do_something_with_input(lists:reverse(Acc)),
    handle_input(Rest, [], Fn, Port);
handle_input([Modlist|Rest], Acc, _, Port) when is_list(Modlist) ->
    handle_input(Rest, Acc, choose_keymap(Modlist), Port);
handle_input([Keycode|Rest], Acc, Fn, Port) ->
    handle_input(Rest, Fn(Keycode, Acc), Fn, Port);
handle_input([], Acc, Fn, Port) ->
    receive
        {input, Port, InputList} -> handle_input(InputList, Acc, Fn, Port)
    end.
{% endhighlight %}
{: __}

I left keycode mappings to be done in Erlang because doing them in the driver (= in C) doesn't sound too appealing: either having to somehow represent the (fairly complicated) static mappings in the driver code or having to read the mappings from a file in some format; both cases better left to the higher level language. We could of course read the keymap file in Erlang and then pass the configuration on to the driver, but that too means complicating the driver. I think it better to keep the driver as small and simple as possible, especially since the following warning can be found all over the driver documentation: `"This stuff runs as an extension of the Beam emulator, and thus does not enjoy any of the safeguards of Erlang. You're writing plain old C here, and you alone are responsible for the suffering it causes you, your Erlang program and everyone around you"`. I'm paraphrasing, but it's close enough.

> The line we're drawing in our design is rather arbitrary: there is no reason why our driver has to eat and maintain the current state of the various modifier keys and transform enter keys to 'end' atoms. We could just as well send a message with current modifier state on startup and then just pass all keycodes as messages. Or we could buffer characters with their modifier flags (each code and its modifiers would fit in a 32 bit integer) up to a complete line in our driver and only send complete reads. Depending on the requirements of the case, you might opt for a different design.

### Driver initialization

Most of what our driver is going to do has already been covered: it will read input events from the device, transform them into appropriate Erlang terms and then forward them to an Erlang process. While initializing the driver, however, we have to be able to actively query as to the current state of the device, instead of merely waiting for future events.

As we are starting our driver, after some basic checks and modifications (verify that the device is actually a device file, make the socket non-blocking so that a `read()` call will never block), we'll have to check if any modifier buttons are currently depressed. We also have to check the current state of caps lock and num lock. This has to be done because, as discussed previously, we are the keepers of state for the device and because the driver isn't going to replay the entire history of the device after our `open()` call.

According to the [header file](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input.h){: rel="nofollow" target="_blank" __} and this [article](https://www.linuxjournal.com/article/6429){: rel="nofollow" target="_blank" __}, we can check the current state of keys and leds on a keyboard using [`ioctl()`](https://linux.die.net/man/2/ioctl){: rel="nofollow" target="_blank" __} with [`EVIOCGKEY()`](https://github.com/torvalds/linux/blob/e93c9c99a629c61837d5a7fc2120cd2b6c70dbdd/include/uapi/linux/input.h#L169){: rel="nofollow" target="_blank" __} and [`EVIOCGLED()`](https://github.com/torvalds/linux/blob/e93c9c99a629c61837d5a7fc2120cd2b6c70dbdd/include/uapi/linux/input.h#L170){: rel="nofollow" target="_blank" __} arguments. Each of these takes a buffer as their argument and then proceeds to set the bits representing depressed keys and leds that are currently on. Thus if the bit in the index of the value of `KEY_A` is `1`, `A` button is currently depressed. Do note, that instead of counting from the most significant bit down, bits in a buffer byte are counted from the least significant bit up. For example, the value `KEY_ESC` is `1` and the bit representing it can be checked with the expression `buf[0] & (1<<1)`. `KEY_A` (the value of which is `30`) can be checked with `buf[3] & (1 << 6)`.

> I'm assuming that checking the caps lock and num lock leds are a reasonable indication as to their status. Because the buttons act as toggles, whether or not they're depressed doesn't conclusively tell us anything about which state they are in. Note, however, that due to the global nature of caps lock on Linux ([highlighted](#{{ "USB barcode (and other) readers" | slugify}}) at the beginning of this piece), it is possible that the state of the caps lock led on the device has been changed by a caps lock toggle on a different device. Thus it might turn out that, instead of waiting for `KEY_CAPSLOCK` events on the device, we should actually check for `EV_LED` with `LED_CAPSL` and `LED_NUML` codes to get the correct picture of the caps lock and num lock states. Alternatively, if we know that our device never relies on either of the toggle keys to work correctly, we could discard with the states for both these keys altogether, as they would not be needed and will only serve to mess up the input. Finally we can also choose to discard `'capslock'` and `'numlock'` atoms if they appear in a modifier list.

{% highlight c %}
// Figure out which modifier keys and which status leds are currently
// depressed/on
static int evdev_drv_initial_state(struct evdev_drv_state *state)
{
	char buf[CUR_STATE_BUF_LEN];

	memset(buf, 0x00, sizeof(buf));

	if (ioctl(state->fd, EVIOCGKEY(sizeof(buf)), buf) == -1)
		return -1;

	for (int i = 0; i < KEY_MAX; ++i) {
		// Skip if the button is not depressed
		if (!(buf[i / CHAR_BIT] & (1 << (i % CHAR_BIT))))
			continue;

		switch (i) {
		// If i matches a keycode, set the flag
		CASEFLAG(KEY_LEFTSHIFT,  LSHIFT)
		CASEFLAG(KEY_RIGHTSHIFT, RSHIFT)
		CASEFLAG(KEY_LEFTALT,    LALT)
		CASEFLAG(KEY_RIGHTALT,   RALT)
		CASEFLAG(KEY_LEFTCTRL,   LCTRL)
		CASEFLAG(KEY_RIGHTCTRL,  RCTRL)
		CASEFLAG(KEY_LEFTMETA,   LMETA)
		CASEFLAG(KEY_RIGHTMETA,  LMETA)
		default:
			break;
		}
	}

	memset(buf, 0x00, sizeof(buf));

	if (ioctl(state->fd, EVIOCGLED(sizeof(buf)), buf) == -1)
		return -1;

	for (int i = 0; i < LED_MAX; ++i) {
		// Skip if the led is not on
		if (!(buf[i / CHAR_BIT] & (1 << (i % CHAR_BIT))))
			continue;

		switch (i) {
		// If i matches a keycode, set the flag
		CASEFLAG(LED_NUML,  NUMLOCK)
		CASEFLAG(LED_CAPSL, CAPSLOCK)
		default:
			break;
		}
	}

	return 0;
}
{% endhighlight %}
{: **}

In this example we don't do anything about non-modifier keys that might be depressed at the time of the `ioctl()` call. It is possible, however, that one or more non-modifier keys turn out to be depressed. In such a case we would not be able to tell anything about the order in which they were pressed, or how many button presses might have preceded them. This introduces the possibility that the first input acquired from a device could be truncated, as our `open()` call might have occurred in between `EV_KEY` events. I elected not to treat this as a special case because there is ultimately no guarantee that _any_ read from the device – be it the first one or any that follow it – is valid: instead of what we're expecting, the read might be the closest barcode that happens to capture the eye of a wandering/bored mind. We always have to validate the input and that validation should catch cases where the input is truncated.

### Returning the input

We have a few different ways to return data from the driver. Had we decided to buffer everything up to an enter key in the driver, we could have then sent the values and their modifiers as a buffer to the emulator with [`driver_output()`](http://erlang.org/doc/man/erl_driver.html#driver_output){: rel="nofollow" target="_blank" __} and could then match the input with Erlang bit syntax. We'd need to explicitly handle value endianness in the driver – Erlang, network oriented as ever, assumes big-endian values in bit syntax –, but [`htons()`](https://linux.die.net/man/3/htons){: rel="nofollow" target="_blank" __} (or just plain byte wrangling) would have been perfectly suitable for that.

{% highlight erlang %}
handle_input(<<Keycode:16, Modbits:16/bits, Rest/bits>>, Acc, Port) ->
    handle_input(Rest, map_keycode(Keycode, Modbits), Port);
handle_input(<<>>, Acc, Port) ->
    do_something_with_input(Acc),
    receive
        {Port, {data, Input}} -> handle_input(Input, [], Port)
    end.
{% endhighlight %}

I'm personally more fond of returning native Erlang terms from the NIFs/drivers I've written, so I decided to use [`erl_drv_output_term()`](http://erlang.org/doc/man/erl_driver.html#erl_drv_output_term){: rel="nofollow" target="_blank" __} instead – although I'll freely admit that constructing lists and tuples using the reverse polish notation as required by `erl_drv_output_term()` is a bit of a pain.

### Sample driver input

Here's a small input sample from a barcode reader for the string `"$EmailAdDrEsS!?"@example.com` in the format produced by our driver.

{% highlight erlang %}
Shell got {input, #Port<0.11>, [[],[shift],3]}  % KEY_2     - $"
Shell got {input, #Port<0.11>, [[],[shift],5]}  % KEY_4     - $$
Shell got {input, #Port<0.11>, [[],[shift],18]} % KEY_E     - $E
Shell got {input, #Port<0.11>, [[],50]}         % KEY_M     - $m
Shell got {input, #Port<0.11>, [[],30]}         % KEY_A     - $a
Shell got {input, #Port<0.11>, [[],23]}         % KEY_I     - $i
Shell got {input, #Port<0.11>, [[],38]}         % KEY_L     - $l
Shell got {input, #Port<0.11>, [[],[shift],30]} % KEY_A     - $A
Shell got {input, #Port<0.11>, [[],32]}         % KEY_D     - $d
Shell got {input, #Port<0.11>, [[],[shift],32]} % KEY_D     - $D
Shell got {input, #Port<0.11>, [[],19]}         % KEY_R     - $r
Shell got {input, #Port<0.11>, [[],[shift],18]} % KEY_E     - $E
Shell got {input, #Port<0.11>, [[],31]}         % KEY_S     - $s
Shell got {input, #Port<0.11>, [[],[shift],31]} % KEY_S     - $S
Shell got {input, #Port<0.11>, [[],[shift],2]}  % KEY_1     - $!
Shell got {input, #Port<0.11>, [[],[shift],12]} % KEY_MINUS - $?
Shell got {input, #Port<0.11>, [[],[shift],3]}  % KEY_2     - $"
Shell got {input, #Port<0.11>, [[],[alt_gr],3]} % KEY_2     - $@
Shell got {input, #Port<0.11>, [[],18]}         % KEY_E     - $e
Shell got {input, #Port<0.11>, [[],45]}         % KEY_X     - $x
Shell got {input, #Port<0.11>, [[],30]}         % KEY_A     - $a
Shell got {input, #Port<0.11>, [[],50]}         % KEY_M     - $m
Shell got {input, #Port<0.11>, [[],25]}         % KEY_P     - $p
Shell got {input, #Port<0.11>, [[],38]}         % KEY_L     - $l
Shell got {input, #Port<0.11>, [[],18]}         % KEY_E     - $e
Shell got {input, #Port<0.11>, [[],52]}         % KEY_DOT   - $.
Shell got {input, #Port<0.11>, [[],46]}         % KEY_C     - $c
Shell got {input, #Port<0.11>, [[],24]}         % KEY_O     - $o
Shell got {input, #Port<0.11>, [[],50]}         % KEY_M     - $
Shell got {input, #Port<0.11>, [[],'end']}
{% endhighlight %}

You can see that my reader assumes that `"` is produced by `Shift+2`, `$` is produced by `Shift+4`, `!` is produced by `Shift+1`, `?` is produced by `Shift+-` and that `@` is produced by `RightAlt+2`. These more or less line up with the Finnish keyboard – except `Shift+4` actually produces `€` – but are mostly off on Dvorak. Pragmatic, not correct.

### (Re-)Connected devices

You might have already picked on the fact that our driver is rather static. It assumes that you have a prior idea of which devices you are interested in, you know where they can be found and then you start a driver on them. However, USB devices can appear and disappear according to the whims of the user. We will not be tackling the topic of device enumeration, discovery and monitoring in this article, but for those interested, I suggest you take a look at [udev](https://www.freedesktop.org/software/systemd/man/udev.html){: rel="nofollow" target="_blank" __} and [libudev](https://www.freedesktop.org/software/systemd/man/libudev.html){: rel="nofollow" target="_blank" __}. Monitoring, in particular, might prove suitable for implementing via an Erlang driver.

### Captured, not gone

Do remember that although we get the input from the reader in this form, it's also most likely spewing the same input into a terminal/GUI somewhere, provided that you don't somehow exclude the device from acting as input. It is, after all, treated as a normal keyboard. Personally, I don't know one would go about doing this and I've only ever used this method of acquiring reader input on a Raspberry Pi device on which I have disabled the local terminals.

## The dark side of the barcode reader

Keep in mind that while these readers are convenient, plugging in one is like plugging in another keyboard that you don't directly control. It may well work as advertised and only produce the inputs for the barcode, but then again you don't know what might happen. Do note that I'm not insinuating that these devices are a major and ongoing threat, just pointing out that their nature as general USB input devices complicates the assessment compared to, for example, serial barcode readers which are not treated as general input devices by the operating system.

For example, a malicious/modified firmware in one of these devices could well be able to recognize an input format used in online banking and, once in while, alter the details in such a way as to redirect money somewhere else than was intended. Because of this, proper online banking (and other) applications are not going to execute anything without acquiring further verification from the user (just another case proving the value of two factor authentication).

## The end

Thus concludes our small expedition into raw keyboard input. I hope the examples prove instructive and/or helpful, as the documentation about handling raw input devices on Linux tends to be on the sparser side – which may not be completely unexpected, as few application aside from X and Wayland are really expected to interface with them.

### Files

You can download the [source code](/assets/code/erlang_evdev_driver.zip) for the driver application.

## Footnotes

### Footnote 1

To get/inspect all input events from a particular device, you can do the following.

{% highlight erlang %}
Cmd = io_lib:format("erlang_evdev_driver ~s", ["/dev/input/event0"]),
Port = open_port({spawn_driver, Cmd}, [binary,stream]),
port_control(Port, 1, []), % You can replace 1 with 0 to return back to the default mode
receive
    {input, Port, {_, _}, _, _, _} = Msg -> Msg
end.
{% endhighlight %}
{: __}

### License

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/80x15.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">Raw keyboard input in Erlang on Linux</span>, and associated code samples, by <a xmlns:cc="http://creativecommons.org/ns#" href="http://www.arv.io/articles/calling-shell-from-erlang" property="cc:attributionName" rel="cc:attributionURL">Lauri Moisio</a> are licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.
