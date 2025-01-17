// SPDX-License-Identifier: MIT

/*
 * This file uses the 8080 emulator to run the test suite (roms in cpu_tests
 * directory). It uses a simple array as memory.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <inttypes.h>

#include "i8080.h"

// memory callbacks
#define MEMORY_SIZE 0x10000
static uint8_t *memory;
static bool test_finished = 0;

static uint8_t rb(void *userdata, const uint16_t addr)
{
	(void)userdata;	// unused
	return memory[addr];
}

static void wb(void *userdata, const uint16_t addr, const uint8_t val)
{
	(void)userdata;	// unused
	memory[addr] = val;
}

static uint8_t port_in(void *userdata, uint8_t port)
{
	(void)port;	// unused
	struct i8080 *const c = (struct i8080 *) userdata;

	uint8_t operation = c->r.eg8[REG_C];

	// print a character stored in E
	if (operation == 2)
		printf("%c", c->r.eg8[REG_E]);

	// print from memory at (DE) until '$' char
	else if (operation == 9) {
		uint16_t addr = c->r.eg16[REG_DE];
		do {
			printf("%c", rb(c, addr++));
		} while (rb(c, addr) != '$');
	}

	return 0xFF;
}

static void port_out(void *userdata, uint8_t port, uint8_t value)
{
	(void)userdata;	// unused
	(void)port;	// unused
	(void)value;	// unused
	test_finished = 1;
}

static int load_file(const char *filename, uint16_t addr)
{
	FILE *f = fopen(filename, "rb");
	if (f == NULL) {
		fprintf(stderr, "error: can't open file '%s'.\n", filename);
		return 1;
	}
	// file size check:
	fseek(f, 0, SEEK_END);
	size_t file_size = ftell(f);
	rewind(f);

	if (file_size + addr >= MEMORY_SIZE) {
		fprintf(stderr, "error: file %s can't fit in memory.\n",
			filename);
		return 1;
	}
	// copying the bytes in memory:
	size_t result = fread(&memory[addr], sizeof(uint8_t), file_size, f);
	if (result != file_size) {
		fprintf(stderr, "error: while reading file '%s'\n", filename);
		return 1;
	}

	fclose(f);
	return 0;
}

/*
 * runs a program, handling CALL 5 call to output test results to standard
 * output
 */
static void run_test(struct i8080 *const c, const char *filename,
		     uint64_t cyc_expected)
{
	i8080_init(c);
	c->userdata = c;
	c->read_byte = rb;
	c->write_byte = wb;
	c->port_in = port_in;
	c->port_out = port_out;
	memset(memory, 0, MEMORY_SIZE);

	if (load_file(filename, 0x100) != 0)
		return;

	printf("*** TEST: %s\n", filename);

	c->pc = 0x100;
	// inject "out 0h" at 0x0000 (signal to stop the test)
	memory[0x0000] = 0xD3;
	memory[0x0001] = 0x00;

	// inject "in 0h" at 0x0005 (signal to output some characters)
	memory[0x0005] = 0xDB;
	memory[0x0006] = 0x00;
	memory[0x0007] = 0xC9;

	uint64_t nb_instructions = 0;
	uint64_t cyc_count = 0;

	test_finished = 0;
	while (!test_finished) {
		nb_instructions += 1;

		/*
		 * uncomment the following line to have a debug output of 
		 * the 8080 state.  Warning: will output multiple GB of data
		 * for the whole test suite
		 */
#ifdef I8080_DEBUG_OUTPUT
		i8080_debug_output(c);
#endif
		i8080_step(c);
		cyc_count += c->cyc;
		c->cyc = 0;
	}

	int64_t diff = cyc_expected - cyc_count;
	printf("\n*** %"PRIu64" instructions executed on %"PRIu64" cycles"
	       " (expected=%"PRIu64", diff=%"PRIi64")\n\n",
	       nb_instructions, cyc_count, cyc_expected, diff);
}

int main(void)
{
	memory = malloc(MEMORY_SIZE * sizeof(uint8_t));
	if (memory == NULL)
		return 1;

	struct i8080 cpu;
	run_test(&cpu, "cpu_tests/TST8080.COM", 4924LU);
	run_test(&cpu, "cpu_tests/CPUTEST.COM", 255653383LU);
	run_test(&cpu, "cpu_tests/8080PRE.COM", 7817LU);
	run_test(&cpu, "cpu_tests/8080EXM.COM", 23803381171LU);

	free(memory);

	return 0;
}
