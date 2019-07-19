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

/*
 * Define a list of related opcodes which will be used with a lookup table
 * to allow a consolidation of the code to process each set of instructions.
 *
 * e.g. MOV_R_R will be used for all register to register MOV instructions.
 */
enum {
	/* Move register to register */
	MOV_R_R,
	/* Move immediate to register */
	MVI_R_N,
	/* Register arithmetic */
	ADD_R,
	ADC_R,
	SUB_R,
	SBB_R,
	ANA_R,
	XRA_R,
	ORA_R,
	CMP_R,
	/* Increment/Decrement */
	INR_R,
	DCR_R,
	/* Conditionals */
	RCC,
	JCC_NN,
	CCC_NN,
	/* RST */
	RST_P,
	/* Register pair */
	LXI_RR_NN,
	STAX_RR,
	INX_RR,
	DAD_RR,
	LDAX_RR,
	DCX_RR,
	POP_RR,
	PUSH_RR,
	/* Immediate arithmetic */
	ADI_N,
	ACI_N,
	SUI_N,
	SBI_N,
	ANI_N,
	XRI_N,
	ORI_N,
	CPI_N,
	/* Indrect HL arithmetic */
	ADD_M,
	ADC_M,
	SUB_M,
	SBB_M,
	ANA_M,
	XRA_M,
	ORA_M,
	CMP_M,
	/* Indrect HL increment/decrement */
	INR_M,
	DCR_M,
	/* Indirect HL moves */
	MOV_M_R,
	MOV_R_M,
	MVI_M_N,
	/* Other stuff */
	CALL_NN_UNDOCUMENTED,
	CALL_NN,
	CMA,
	CMC,
	DAA,
	DI,
	EI,
	HALT,
	IN_N,
	JMP_NN_UNDOCUMENTED,
	JMP_NN,
	LDA_NN,
	LHLD_NN,
	NOP_UNDOCUMENTED,
	NOP,
	OUT_N,
	PCHL,
	RET,
	RET_UNDOCUMENTED,
	RLC,
	RAL,
	RRC,
	RAR,
	SHLD_NN,
	SPHL,
	STA_NN,
	STC,
	XCHG,
	XTHL,
	INSN_MAX	/* Can be used for array definitions */
};

/*
 * Define the flag bits for the F register
 */
enum {
	FLAG_CARRY,
	FLAG_RESERVED_1,
	FLAG_PARITY,
	FLAG_RESERVED_2,
	FLAG_AUX_CARRY,
	FLAG_RESERVED_3,
	FLAG_ZERO,
	FLAG_SIGN
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
	uint8_t *reg8_table[8];
	uint16_t *reg16_sp[4];

	// memory + io interface
	void *userdata;			// general purpose pointer for the user
	uint8_t(*read_byte) (void *, uint16_t);
	void (*write_byte)(void *, uint16_t, uint8_t);
	uint8_t(*port_in) (void *, uint8_t);
	void (*port_out)(void *, uint8_t, uint8_t);
};

void i8080_init(struct i8080 *const c);
void i8080_step(struct i8080 *const c);
void i8080_interrupt(struct i8080 *const c, const uint8_t opcode);

#ifdef I8080_DEBUG_OUTPUT
void i8080_debug_output(struct i8080 *const c);
#endif

#endif // I8080_I8080_H_
