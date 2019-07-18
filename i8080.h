// SPDX-License-Identifier: MIT

#ifndef I8080_I8080_H_
#define I8080_I8080_H_

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <endian.h>

/*
 * The relationship between the two 8-bit registers and the 16-bit register
 * pair (e.g., H, L and HL) differs depending on the byte order of the
 * host system
 */
#if __BYTE_ORDER == __BIG_ENDIAN

#define REG_B   0
#define REG_C   1
#define REG_D   2
#define REG_E   3
#define REG_H   4
#define REG_L   5
#define REG_A   6
#define REG_F   7	// Not using this yet
#define REG_S   8
#define REG_P   9

#else /* __LITTLE_ENDIAN */

#define REG_B   1
#define REG_C   0
#define REG_D   3
#define REG_E   2
#define REG_H   5
#define REG_L   4
#define REG_A   7
#define REG_F   6	// Not using this yet
#define REG_S   9
#define REG_P   8

#endif /* __BYTE_ORDER */

#define REG_BC  0
#define REG_DE  1
#define REG_HL  2
#define REG_AF  3
#define REG_SP  4

struct i8080 {
	union {
		uint8_t eg8[10];	// B, C, D, E, H, L, A, F, S, P
		uint16_t eg16[10];	// BC, DE, HL, AF, SP
	} r;
	uint16_t pc;			// program counter
	bool sf, zf, hf, pf, cf, iff;	// flags:sign, zero, half-carry, parity,
					// carry, interrupt flip-flop
	unsigned long cyc;		// cycle count
	bool halted;
	bool interrupt_pending;
	uint8_t interrupt_vector;
	uint8_t interrupt_delay;

	// memory + io interface
	void *userdata;			// general purpose pointer for the user
	 uint8_t(*read_byte) (void *, uint16_t);
	void (*write_byte)(void *, uint16_t, uint8_t);
	 uint8_t(*port_in) (void *, uint8_t);
	void (*port_out)(void *, uint8_t, uint8_t);
};

void i8080_init(struct i8080 *const c);
void i8080_step(struct i8080 *const c);
void i8080_interrupt(struct i8080 *const c, uint8_t opcode);
void i8080_debug_output(struct i8080 *const c);

#endif // I8080_I8080_H_
