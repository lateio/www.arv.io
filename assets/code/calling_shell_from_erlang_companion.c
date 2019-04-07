/*
 * http://www.arv.io/assets/code/calling_shell_from_erlang_companion.c
 *
 * Part of http://www.arv.io/articles/calling-shell-from-erlang
 *
 * Intended for use with function example_companion/0 from
 * http://www.arv.io/assets/code/calling_shell_from_erlang.erl
 */

#include <stdlib.h> // size_t
#include <unistd.h> // read(), write(), STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO
#include <string.h> // strlen()
#include <stdio.h>  // strerror(), dprintf()
#include <errno.h>  // errno

static const char hello[]       = "Meowdy-do, companion ready\n";
static const char loop[]        = "Loop\n";
static const char goodbye[]     = "Goodbye\n";
static const char goodbye_err[] = "Goodbye (standard error)\n";

int main(int argc, const char **argv)
{
	/* printf(hello), puts(hello);
	 * printf(), puts() and other related functions write
	 * to stdout stream, the buffered nature of which makes
	 * the "delivery" of our output over an Erlang port
	 * unpredictable -- suppose Erlang waits to see
	 * the greeting before passing anything to our
	 * standard input, but on our side a function buffers
	 * that output. Erlang'll be waiting for a write we'll
	 * never fulfill (nothing here but our main thread)
	 * and we'll be waiting for a write Erlang'll never fulfill.
	 *
	 * Using write() and the standard file descriptors,
	 * our data will be visible in Erlang like we expect
	 * it to be.
	 */
	write(STDOUT_FILENO, hello, sizeof(hello) - 1);

	// Echo command line arguments back at Erlang
	while (++argv, --argc > 0)
		write(STDOUT_FILENO, *argv, strlen(*argv));

	int rd;
	char buf[32];

	while ((rd = read(STDIN_FILENO, buf, sizeof(buf))) > 0)
		if (write(STDOUT_FILENO, loop, sizeof(loop) - 1) < 0)
			dprintf(STDERR_FILENO, "loop write error: %s\n", strerror(errno));

	if (rd < 0)
		dprintf(STDERR_FILENO, "read error: %s\n", strerror(errno));

	if (write(STDOUT_FILENO, goodbye, sizeof(goodbye) - 1) < 0)
		dprintf(STDERR_FILENO, "write error: %s\n", strerror(errno));

	write(STDERR_FILENO, goodbye_err, sizeof(goodbye_err) - 1);

	return -1; // exit(-1);
}
