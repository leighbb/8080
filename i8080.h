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

enum {
	REG_B,
	REG_C,
	REG_D,
	REG_E,
	REG_H,
	REG_L,
	REG_A,
	REG_F,		// Not using this yet
	REG_S,
	REG_P
};

#else /* __LITTLE_ENDIAN */

enum {
	REG_C,
	REG_B,
	REG_E,
	REG_D,
	REG_L,
	REG_H,
	REG_F,		// Not using this yet
	REG_A,
	REG_P,
	REG_S
};
#endif /* __BYTE_ORDER */

enum {
	REG_BC,
	REG_DE,
	REG_HL,
	REG_AF,
	REG_SP
};

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
