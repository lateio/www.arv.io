/*
 * http://www.arv.io/assets/code/calling_shell_from_erlang_reader.c
 *
 * Part of http://www.arv.io/articles/calling-shell-from-erlang
 *
 * Intended for use with
 *         http://www.arv.io/assets/code/calling_shell_from_erlang_timer.c
 * and
 *         http://www.arv.io/assets/code/calling_shell_from_erlang_writer.c
 */

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

static char buf[16384];

int main(void)
{
	int rd;
	size_t total = 0;

	for (int i = 0; i < 5; ++i) {
		if ((rd = read(STDIN_FILENO, buf, sizeof(buf))) < 0)
			exit(-1);
		total += rd;
		fprintf(stderr, "Reader: read %d additional bytes for a total of %zu\n", rd, total);
	}

	fprintf(stderr, "Reader: napping for 5 seconds before exit()\n");

	sleep(5);

	fprintf(stderr, "Reader: stopping\n");

	return 0;
}
