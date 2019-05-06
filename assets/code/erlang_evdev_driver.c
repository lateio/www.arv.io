// Part of http://www.arv.io/articles/raw-keyboard-input-erlang-linux

#include <erl_driver.h>

#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <string.h>
#include <sys/stat.h>
#include <linux/input.h>

#define ARRAY_SIZE(x) (sizeof(x)/sizeof(x[0]))

struct evdev_drv_state {
#define LSHIFT   (1<<0)
#define RSHIFT   (1<<1)
#define SHIFT    (LSHIFT | RSHIFT)
#define CAPSLOCK (1<<2)
#define NUMLOCK  (1<<3)
#define LCTRL    (1<<4)
#define RCTRL    (1<<5)
#define CTRLKEY  (LCTRL | RCTRL)
#define LALT     (1<<6)
#define RALT     (1<<7) // AltGr
#define LMETA    (1<<8)
#define RMETA    (1<<9)
#define META     (LMETA | RMETA)
#define INSPECT  (1<<10)
	int flags; // Store flags describing the device state

	int fd;          // Device fd
	ErlDrvPort port; // Erlang port
};

#define CUR_STATE_BUF_LEN (((KEY_MAX > LED_MAX) ? KEY_MAX : LED_MAX) / CHAR_BIT + 1)

static int evdev_drv_initial_state(struct evdev_drv_state *state)
{
	char buf[CUR_STATE_BUF_LEN];

	memset(buf, 0x00, sizeof(buf));

	if (ioctl(state->fd, EVIOCGKEY(sizeof(buf)), buf) == -1)
		return -1;

#define CASEFLAG(KEYCODE, FLAG)\
case KEYCODE:\
	state->flags |= FLAG;\
	break;

	for (int i = 0; i < KEY_MAX; ++i) {
		// Skip if the button is not depressed
		if (!(buf[i / CHAR_BIT] & (1 << (i % CHAR_BIT))))
			continue;

		switch (i) {
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
		CASEFLAG(LED_NUML,  NUMLOCK)
		CASEFLAG(LED_CAPSL, CAPSLOCK)
		default:
			break;
		}
	}

	return 0;
}

static ErlDrvData evdev_drv_start(ErlDrvPort port, char *command)
{
	for (; *command != '\0' && *command != ' '; ++command);
	if (*command == '\0')
		return ERL_DRV_ERROR_BADARG;

	command += 1; // Skip space

	int fd;

try_again:
	if ((fd = open(command, O_RDONLY | O_CLOEXEC)) < 0)
		switch (errno) {
		case EINTR:
			goto try_again;
			break;
		default:
			goto error_errno;
			break;
		}

	struct stat st;
	if (fstat(fd, &st))
		goto error_fd_errno;

	if (!S_ISCHR(st.st_mode))
		goto error_fd;

	// Make the socket non-blocking
	int flags;
	if ((flags = fcntl(fd, F_GETFL)) == -1)
		goto error_fd_errno;

	if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1)
		goto error_fd_errno;

	struct evdev_drv_state *state = driver_alloc(sizeof(*state));
	if (!state)
		goto error_fd;

	state->fd = fd;
	state->port = port;
	state->flags = 0;

	if (evdev_drv_initial_state(state))
		goto error_state_fd_errno;

	ErlDrvEvent event = (void *)(long)state->fd;
        if (driver_select(state->port, event, ERL_DRV_READ | ERL_DRV_USE, 1))
                goto error_state_fd;

	return (ErlDrvData)state;


error_state_fd:
	driver_free(state);
error_fd:
	if (close(fd))
		switch (errno) {
		case EINTR:
			goto error_fd;
			break;
		default:
			break;
		}
	return ERL_DRV_ERROR_GENERAL;


	int errno_stash;
error_state_fd_errno:
	errno_stash = errno;
	driver_free(state);
	errno = errno_stash;
error_fd_errno:
	errno_stash = errno;
	if (close(fd))
		switch (errno) {
		case EINTR:
			errno = errno_stash;
			goto error_fd_errno;
			break;
		default:
			break;
		}
	errno = errno_stash;
error_errno:
	return ERL_DRV_ERROR_ERRNO;
}

static void evdev_drv_stop(ErlDrvData data)
{
	struct evdev_drv_state *state = (struct evdev_drv_state *)data;

	driver_select(state->port, (ErlDrvEvent)(long)state->fd, ERL_DRV_USE, 0);

	driver_free(data);
}

static void evdev_drv_stop_select(ErlDrvEvent event, void *reserved)
{
	// Close the device fd
	close((int)event);
}

static void evdev_drv_output(ErlDrvData data, char *buf, ErlDrvSizeT len)
{
	return;
}

static ErlDrvSSizeT evdev_drv_control(
	ErlDrvData data,
	unsigned int command,
	char *buf,
	ErlDrvSizeT buf_len,
	char **rbuf,
	ErlDrvSizeT rbuf_len)
{
	struct evdev_drv_state *state = (struct evdev_drv_state *)data;

	if (!command) {
		state->flags &= ~INSPECT;
	} else {
		state->flags |= INSPECT;
	}

	return 0;
}

static void init_input_tuple(ErlDrvTermData *spec, ErlDrvPort port)
{
	const ErlDrvTermData start[] = {
		ERL_DRV_ATOM, driver_mk_atom("input"),
		ERL_DRV_PORT, driver_mk_port(port)
	};

	memcpy(spec, start, sizeof(start));
}

static void term_send_input_tuple(int spec_off, int list_len, ErlDrvTermData *spec, struct evdev_drv_state *state)
{
	const ErlDrvTermData end[] = {
		ERL_DRV_NIL,
		ERL_DRV_LIST, list_len + 1,
		ERL_DRV_TUPLE, 3
	};

	memcpy(spec + spec_off, end, sizeof(end));

	erl_drv_output_term(driver_mk_port(state->port), spec, spec_off + 5);
}

static const struct flag_atom {
	const int flag;
	char *atom;
} flag_atoms[] = {
	{SHIFT,    "shift"    },
	{CAPSLOCK, "capslock" },
	{NUMLOCK,  "numlock"  },
	{CTRLKEY,  "control"  },
	{LALT,     "alt"      },
	{RALT,     "alt_gr"   },
	{META,     "meta"     }
};

static void add_modlist(int flags, int *spec_off, ErlDrvTermData *spec)
{
	int start = *spec_off;

	for (int i = 0; i < ARRAY_SIZE(flag_atoms); ++i) {
		if (!(flags & flag_atoms[i].flag))
			continue;

		spec[*spec_off] = ERL_DRV_ATOM;
		spec[*spec_off + 1] = driver_mk_atom(flag_atoms[i].atom);
		*spec_off += 2;
	}

	const ErlDrvTermData modlist_end[] = {
		ERL_DRV_NIL,
		ERL_DRV_LIST, (*spec_off - start) / 2 + 1
	};
	memcpy(spec + *spec_off, modlist_end, sizeof(modlist_end));
	*spec_off += ARRAY_SIZE(modlist_end);
}

static void evdev_drv_ready_input_forward_useful(ErlDrvData data, ErlDrvEvent event)
{
	struct evdev_drv_state *state = (struct evdev_drv_state *)data;

	struct input_event buf[16];

	int rd;
	rd = read(state->fd, buf, sizeof(buf));
	if (rd < 0) {
		switch (errno) {
		case EAGAIN: // There was no input after all, keep on trucking
			return;
		case EINTR: // Dunno if the thread can receive signals, but it doesn't hurt to prepare
			return;
		case ENODEV: // Device was (prolly) unplugged
			driver_failure_atom(state->port, "unplugged");
			return;
		default:
			driver_failure_posix(state->port, errno);
			return;
		}
	}

	if (rd % sizeof(buf[0])) {
		driver_failure_atom(state->port, "non_evdev_device");
		return;
	}

	ErlDrvTermData spec[128];
	int spec_off = 4,      // We always have at least the tuple start
	    spec_list_len = 1, // We always have at least the initial modlist
	    keycodes      = 0,
	    modlists      = 0;

	init_input_tuple(spec, state->port);
	add_modlist(state->flags, &spec_off, spec);

	for (int i = 0; i < (rd / sizeof(buf[0])); ++i) {
		if (buf[i].type != EV_KEY)
			continue;

		if (spec_off >= ARRAY_SIZE(spec) - (3 + ARRAY_SIZE(flag_atoms) * 2) + 2 + 5) {
			// If our spec cannot fit the largest possible
			// input (full modlist, 'end' atom and tuple end),
			// send the current spec and start off fresh
			term_send_input_tuple(spec_off, spec_list_len, spec, state);
			spec_off = 4;      // We always have at least the tuple start
			spec_list_len = 1; // We always have at least the initial modlist
			modlists = keycodes = 0;
			init_input_tuple(spec, state->port);
			add_modlist(state->flags, &spec_off, spec);
		}

#define DOUBLEKEY_CASE(KEYCODE1, KEYCODE2, FLAG1, FLAG2)\
case KEYCODE1:\
case KEYCODE2:\
	flagmask = (buf[i].code == KEYCODE1) ? FLAG1 : FLAG2;\
	switch (buf[i].value) {\
	case 0:\
		state->flags &= ~flagmask;\
		break;\
	case 1:\
		state->flags |= flagmask;\
		break;\
	default:\
		continue;\
	}\
	add_modlist(state->flags, &spec_off, spec);\
	spec_list_len += 1;\
	modlists += 1;\
	break;

#define TOGGLEKEY_CASE(KEYCODE, FLAG)\
case KEYCODE:\
	if (buf[i].value != 1)\
		continue;\
	state->flags ^= FLAG;\
	add_modlist(state->flags, &spec_off, spec);\
	spec_list_len += 1;\
	modlists += 1;\
	break;

		int flagmask;
		switch (buf[i].code) {
		DOUBLEKEY_CASE(KEY_LEFTALT,   KEY_RIGHTALT,   LALT,   RALT)
		DOUBLEKEY_CASE(KEY_LEFTSHIFT, KEY_RIGHTSHIFT, LSHIFT, RSHIFT)
		DOUBLEKEY_CASE(KEY_LEFTCTRL,  KEY_RIGHTCTRL,  LCTRL,  RCTRL)
		DOUBLEKEY_CASE(KEY_LEFTMETA,  KEY_RIGHTMETA,  LMETA,  RMETA)

		TOGGLEKEY_CASE(KEY_CAPSLOCK, CAPSLOCK)
		TOGGLEKEY_CASE(KEY_NUMLOCK,  NUMLOCK)

		case KEY_ENTER:
		case KEY_KPENTER:
			if (buf[i].value != 1)
				continue;

			spec[spec_off] = ERL_DRV_ATOM;
			spec[spec_off + 1] = driver_mk_atom("end");
			spec_list_len += 1;
			spec_off += 2;
			keycodes += 1; // Treat 'end' as a keycode, since it'll have to get sent
			break;
		default:
			switch (buf[i].value) {
			case 0:
				continue;
			case 1:
				break;
			case 2:
			default:
				driver_failure_atom(state->port, "key_repeat");
				return;
			}

			spec[spec_off] = ERL_DRV_UINT;
			spec[spec_off + 1] = (unsigned int)buf[i].code;
			spec_list_len += 1;
			spec_off += 2;
			keycodes += 1;
		}
	}

	// Don't send empty messages (ie. messages not containing any keycodes)
	if (keycodes)
		term_send_input_tuple(spec_off, spec_list_len, spec, state);
}

static void evdev_drv_ready_input_forward_all(ErlDrvData data, ErlDrvEvent event)
{
	// {input, Port :: port, Timestamp :: {integer(), integer()}, Type :: atom(), Code :: integer(), Value :: integer()}
	struct evdev_drv_state *state = (struct evdev_drv_state *)data;

	struct input_event buf[16];

	int rd;
	rd = read(state->fd, buf, sizeof(buf));
	if (rd < 0) {
		switch (errno) {
		case EAGAIN: // There was no input after all, keep on trucking
			return;
		case EINTR: // Dunno if the thread can receive signals, but it doesn't hurt to prepare
			return;
		case ENODEV: // Device was (prolly) unplugged
			driver_failure_atom(state->port, "unplugged");
			return;
		default:
			driver_failure_posix(state->port, errno);
			return;
		}
	}

	if (rd % sizeof(buf[0])) {
		driver_failure_atom(state->port, "non_evdev_device");
		return;
	}

	ErlDrvTermData spec[18];

	const struct type_atom {
		unsigned int type;
		char *atom;
	} type_atoms[] = {
		{ EV_SYN,       "syn"       },
		{ EV_KEY,       "key"       },
		{ EV_REL,       "rel"       },
		{ EV_ABS,       "abs"       },
		{ EV_MSC,       "msc"       },
		{ EV_SW,        "sw"        },
		{ EV_LED,       "led"       },
		{ EV_SND,       "snd"       },
		{ EV_REP,       "rep"       },
		{ EV_FF,        "ff"        },
		{ EV_PWR,       "pwr"       },
		{ EV_FF_STATUS, "ff_status" }
	};

	for (int i = 0; i < (rd / sizeof(buf[0])); ++i) {
		const ErlDrvTermData tuple_head[] = {
			ERL_DRV_ATOM,  driver_mk_atom("input"),
			ERL_DRV_PORT,  driver_mk_port(state->port),
			ERL_DRV_UINT,  buf[i].input_event_sec,
			ERL_DRV_UINT,  buf[i].input_event_usec,
			ERL_DRV_TUPLE, 2,
			ERL_DRV_ATOM
		};
		memcpy(spec, tuple_head, sizeof(tuple_head));

		int j;
		for (j = 0; i < ARRAY_SIZE(type_atoms); ++j) {
			if (type_atoms[j].type != buf[i].type)
				continue;
			spec[ARRAY_SIZE(tuple_head)] = driver_mk_atom(type_atoms[j].atom);
			break;
		}
		if (j == ARRAY_SIZE(type_atoms))
			spec[ARRAY_SIZE(tuple_head)] = driver_mk_atom("unknown");

		const ErlDrvTermData tuple_tail[] = {
			ERL_DRV_UINT,  (unsigned int)buf[i].code,
			ERL_DRV_UINT,  buf[i].value,
			ERL_DRV_TUPLE, 6
		};
		memcpy(spec + ARRAY_SIZE(tuple_head) + 1, tuple_tail, sizeof(tuple_tail));

		erl_drv_output_term(driver_mk_port(state->port), spec, ARRAY_SIZE(spec));
	}
}

static void evdev_drv_ready_input(ErlDrvData data, ErlDrvEvent event)
{
	if (((struct evdev_drv_state *)data)->flags & INSPECT)
		return evdev_drv_ready_input_forward_all(data, event);
	else
		return evdev_drv_ready_input_forward_useful(data, event);
}

ErlDrvEntry evdev_driver_entry = {
	.driver_name = "erlang_evdev_driver",

	// Config/Compatibility info
	.extended_marker = ERL_DRV_EXTENDED_MARKER,
	.major_version   = ERL_DRV_EXTENDED_MAJOR_VERSION,
	.minor_version   = ERL_DRV_EXTENDED_MINOR_VERSION,
	.driver_flags    = 0,

	// Callbacks
	.start       = evdev_drv_start,
	.stop        = evdev_drv_stop,
	.stop_select = evdev_drv_stop_select,
	.output      = evdev_drv_output,
	.ready_input = evdev_drv_ready_input,
	.control     = evdev_drv_control
};

DRIVER_INIT(erlang_evdev_driver) /* must match name in driver_entry */
{
	return &evdev_driver_entry;
}
