/*
 * http://www.arv.io/assets/code/calling_shell_from_erlang_writer.c
 *
 * Part of http://www.arv.io/articles/calling-shell-from-erlang
 *
 * Intended for use with
 *         http://www.arv.io/assets/code/calling_shell_from_erlang_timer.c
 * and
 *         http://www.arv.io/assets/code/calling_shell_from_erlang_reader.c
 */

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <signal.h>

static const char buf[65536];

static void handle_sigpipe(int sig)
{
	fprintf(stderr, "Writer: SIGPIPE\n");
}

int main(void)
{
	int wr;
	size_t total = 0;

	if (signal(SIGPIPE, handle_sigpipe) == SIG_ERR)
		return -1;

	while (1) {
		if ((wr = write(STDOUT_FILENO, buf, sizeof(buf))) < 0)
			break;
		total += wr;
		fprintf(stderr, "Writer: wrote %d additional bytes for a total of %zu\n", wr, total);
	}

	fprintf(stderr, "Writer: stopping\n");

	return 0;
}
