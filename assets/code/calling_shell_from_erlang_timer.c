/*
 * http://www.arv.io/assets/code/calling_shell_from_erlang_timer.c
 *
 * Part of http://www.arv.io/articles/calling-shell-from-erlang
 *
 * Intended for use with
 *         http://www.arv.io/assets/code/calling_shell_from_erlang_writer.c
 * and
 *         http://www.arv.io/assets/code/calling_shell_from_erlang_reader.c
 */

#include <unistd.h> // STDOUT_FILENO, sleep(), write()
#include <stdio.h>  // fprintf()

int main(void)
{
	int secs = 0;

	while (1) {
		fprintf(stderr, "Timer: %d %s\n", secs, (secs == 1) ? "second" : "seconds");
		secs += 1;

		write(STDOUT_FILENO, "", 1); // trigger SIGPIPE

		sleep(1);
	}

	return 0;
}
