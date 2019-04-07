/*
 * http://www.arv.io/assets/code/calling_shell_from_erlang_maxsize.c
 *
 * Part of http://www.arv.io/articles/calling-shell-from-erlang
 *
 * Intended for use with function footnote_1/0 from
 * http://www.arv.io/assets/code/calling_shell_from_erlang.erl
 */

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>

static char buf[65536];
static int fd;

static void handle_sigpipe(int sig)
{
	dprintf(fd, "SIGPIPE\n");
}

int main(int argc, const char **argv)
{
	int wr, keep_going = 1;
	size_t write_len;

	if ((fd = open(".death note.txt", O_WRONLY | O_CREAT | O_TRUNC , 0600)) < 0)
		return -1;

	if (signal(SIGPIPE, handle_sigpipe) == SIG_ERR)
		return -1;

	// Default to small-write
	if (!(++argv, --argc))
		*argv = "small-write";

	if (!strcmp(*argv, "small-write")) {
		write_len = 256;
	} else if (!strcmp(*argv, "large-write")) {
		write_len = sizeof(buf);
	} else if (!strcmp(*argv, "single")) {
		write_len = 256;
		keep_going = 0;
	} else {
		return -1;
	}

	do {
		if ((wr = write(STDOUT_FILENO, buf, write_len)) < 0)
			break;
		dprintf(fd, "Wrote %d additional bytes\n", wr);
	} while (keep_going);

	dprintf(fd, "Stopping\n");

	close(fd);

	return 0;
}
