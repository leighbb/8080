// SPDX-License-Identifier: MIT

#include <stdlib.h>

#include "i8080.h"
#include "tables.h"

#define SET_ZSP(c, val) do { \
	c->zf = (val) == 0; c->sf = (val) >> 7; c->pf = parity(val); \
} while(0)

/*
 * Opcode bitfield helpers
 */

static inline uint8_t SSS(uint8_t opcode) { return (opcode >> 0) & 7; }
static inline uint8_t DDD(uint8_t opcode) { return (opcode >> 3) & 7; }
static inline uint8_t  RP(uint8_t opcode) { return (opcode >> 4) & 3; }

/*
 * memory helpers (the only four to use `read_byte` and `write_byte` function
 * pointers)
 */

// reads a byte from memory
static inline uint8_t i8080_rb(struct i8080 *const c, const uint16_t addr)
{
	return c->read_byte(c->userdata, addr);
}

// writes a byte to memory
static inline void i8080_wb(struct i8080 *const c, const uint16_t addr,
			    const uint8_t val)
{
	c->write_byte(c->userdata, addr, val);
}

// reads a word from memory
static inline uint16_t i8080_rw(struct i8080 *const c, const uint16_t addr)
{
	return c->read_byte(c->userdata, addr + 1) << 8 |
	       c->read_byte(c->userdata, addr);
}

// writes a word to memory
static inline void i8080_ww(struct i8080 *const c, const uint16_t addr,
			    const uint16_t val)
{
	c->write_byte(c->userdata, addr, val & 0xFF);
	c->write_byte(c->userdata, addr + 1, val >> 8);
}

// returns the next byte in memory (and updates the program counter)
static inline uint8_t i8080_next_byte(struct i8080 *const c)
{
	return i8080_rb(c, c->pc++);
}

// returns the next word in memory (and updates the program counter)
static inline uint16_t i8080_next_word(struct i8080 *const c)
{
	const uint16_t result = i8080_rw(c, c->pc);
	c->pc += 2;
	return result;
}

/*
 * stack helpers
 */

// pushes a value into the stack and updates the stack pointer
static inline void i8080_push_stack(struct i8080 *const c, uint16_t val)
{
	c->r.eg16[REG_SP] -= 2;
	i8080_ww(c, c->r.eg16[REG_SP], val);
}

// pops a value from the stack and updates the stack pointer
static inline uint16_t i8080_pop_stack(struct i8080 *const c)
{
	uint16_t val = i8080_rw(c, c->r.eg16[REG_SP]);
	c->r.eg16[REG_SP] += 2;
	return val;
}

/*
 * opcodes
 */

// returns the parity of byte: 0 if number of 1 bits in `val` is odd, else 1
static inline bool parity(uint8_t val)
{
	uint8_t nb_one_bits = 0;
	for (int i = 0; i < 8; i++) {
		nb_one_bits += ((val >> i) & 1);
	}

	return (nb_one_bits & 1) == 0;
}

/*
 * returns if there was a carry between bit "bit_no" and "bit_no - 1" when
 * executing "a + b + cy"
 */
static inline bool carry(int bit_no, uint8_t a, uint8_t b, bool cy)
{
	int16_t result = a + b + cy;
	int16_t carry = result ^ a ^ b;
	return carry & (1 << bit_no);
}

// adds a value (+ an optional carry flag) to a register
static inline void i8080_add(struct i8080 *const c, uint8_t * const reg,
			     uint8_t val, bool cy)
{
	const uint8_t result = *reg + val + cy;
	c->cf = carry(8, *reg, val, cy);
	c->hf = carry(4, *reg, val, cy);
	SET_ZSP(c, result);
	*reg = result;
}

// substracts a byte (+ an optional carry flag) from a register
static inline void i8080_sub(struct i8080 *const c, uint8_t * const reg,
			     uint8_t val, bool cy)
{
	// https://stackoverflow.com/a/8037485
	i8080_add(c, reg, ~val, !cy);
	c->cf = !c->cf;
}

// adds a word to HL
static inline void i8080_dad(struct i8080 *const c, uint16_t val)
{
	c->cf = ((c->r.eg16[REG_HL] + val) >> 16) & 1;
	c->r.eg16[REG_HL] += val;
}

// increments a byte
static inline uint8_t i8080_inr(struct i8080 *const c, uint8_t val)
{
	const uint8_t result = val + 1;
	c->hf = (result & 0xF) == 0;
	SET_ZSP(c, result);
	return result;
}

// decrements a byte
static inline uint8_t i8080_dcr(struct i8080 *const c, uint8_t val)
{
	const uint8_t result = val - 1;
	c->hf = !((result & 0xF) == 0xF);
	SET_ZSP(c, result);
	return result;
}

/*
 * executes a logic "and" between register A and a byte, then stores the
 * result in register A
 */
static inline void i8080_ana(struct i8080 *const c, uint8_t val)
{
	uint8_t result = c->r.eg8[REG_A] & val;
	c->cf = 0;
	c->hf = ((c->r.eg8[REG_A] | val) & 0x08) != 0;
	SET_ZSP(c, result);
	c->r.eg8[REG_A] = result;
}

/*
 * executes a logic "xor" between register A and a byte, then stores the
 * result in register A
 */
static inline void i8080_xra(struct i8080 *const c, uint8_t val)
{
	c->r.eg8[REG_A] ^= val;
	c->cf = 0;
	c->hf = 0;
	SET_ZSP(c, c->r.eg8[REG_A]);
}

/*
 * executes a logic "or" between register A and a byte, then stores the
 * result in register A
 */
static inline void i8080_ora(struct i8080 *const c, const uint8_t val)
{
	c->r.eg8[REG_A] |= val;
	c->cf = 0;
	c->hf = 0;
	SET_ZSP(c, c->r.eg8[REG_A]);
}

// compares the register A to another byte
static inline void i8080_cmp(struct i8080 *const c, const uint8_t val)
{
	const int16_t result = c->r.eg8[REG_A] - val;
	c->cf = result >> 8;
	c->hf = ~(c->r.eg8[REG_A] ^ result ^ val) & 0x10;
	SET_ZSP(c, result & 0xFF);
}

// sets the program counter to a given address
static inline void i8080_jmp(struct i8080 *const c, uint16_t addr)
{
	c->pc = addr;
}

// Determine condition based on the bitfield in the opcode
static inline bool condition(struct i8080 *const c, const uint8_t opcode)
{
	switch (opcode >> 3 & 7) {
	case 0:
		return c->zf == 0;
	case 1:
		return c->zf == 1;
	case 2:
		return c->cf == 0;
	case 3:
		return c->cf == 1;
	case 4:
		return c->pf == 0;
	case 5:
		return c->pf == 1;
	case 6:
		return c->sf == 0;
	default:
		return c->sf == 1;
	}
}

/*
 * jumps to next address pointed by the next word in memory if a condition
 * is met
 */
static inline void i8080_cond_jmp(struct i8080 *const c, bool condition)
{
	uint16_t addr = i8080_next_word(c);
	if (condition)
		c->pc = addr;
}

// pushes the current pc to the stack, then jumps to an address
static inline void i8080_call(struct i8080 *const c, uint16_t addr)
{
	i8080_push_stack(c, c->pc);
	i8080_jmp(c, addr);
}

// calls to next word in memory if a condition is met
static inline void i8080_cond_call(struct i8080 *const c, bool condition)
{
	uint16_t addr = i8080_next_word(c);
	if (condition) {
		i8080_call(c, addr);
		c->cyc += 6;
	}
}

// returns from subroutine
static inline void i8080_ret(struct i8080 *const c)
{
	c->pc = i8080_pop_stack(c);
}

// returns from subroutine if a condition is met
static inline void i8080_cond_ret(struct i8080 *const c, bool condition)
{
	if (condition) {
		i8080_ret(c);
		c->cyc += 6;
	}
}

// pushes register A and the flags into the stack
static inline void i8080_push_psw(struct i8080 *const c)
{
	// note: bit 3 and 5 are always 0
	uint8_t psw = 0;
	psw |= c->sf << 7;
	psw |= c->zf << 6;
	psw |= c->hf << 4;
	psw |= c->pf << 2;
	psw |= 1 << 1;		// bit 1 is always 1
	psw |= c->cf << 0;
	i8080_push_stack(c, c->r.eg8[REG_A] << 8 | psw);
}

// pops register A and the flags from the stack
static inline void i8080_pop_psw(struct i8080 *const c)
{
	const uint16_t af = i8080_pop_stack(c);
	c->r.eg8[REG_A] = af >> 8;
	const uint8_t psw = af & 0xFF;

	c->sf = (psw >> 7) & 1;
	c->zf = (psw >> 6) & 1;
	c->hf = (psw >> 4) & 1;
	c->pf = (psw >> 2) & 1;
	c->cf = (psw >> 0) & 1;
}

// rotate register A left
static inline void i8080_rlc(struct i8080 *const c)
{
	c->cf = c->r.eg8[REG_A] >> 7;
	c->r.eg8[REG_A] = (c->r.eg8[REG_A] << 1) | c->cf;
}

// rotate register A right
static inline void i8080_rrc(struct i8080 *const c)
{
	c->cf = c->r.eg8[REG_A] & 1;
	c->r.eg8[REG_A] = (c->r.eg8[REG_A] >> 1) | (c->cf << 7);
}

// rotate register A left with the carry flag
static inline void i8080_ral(struct i8080 *const c)
{
	const bool cy = c->cf;
	c->cf = c->r.eg8[REG_A] >> 7;
	c->r.eg8[REG_A] = (c->r.eg8[REG_A] << 1) | cy;
}

// rotate register A right with the carry flag
static inline void i8080_rar(struct i8080 *const c)
{
	const bool cy = c->cf;
	c->cf = c->r.eg8[REG_A] & 1;
	c->r.eg8[REG_A] = (c->r.eg8[REG_A] >> 1) | (cy << 7);
}

/*
 * Decimal Adjust Accumulator: the eight-bit number in register A is adjusted
 * to form two four-bit binary-coded-decimal digits.
 * For example, if A=$2B and DAA is executed, A becomes $31.
 */
static inline void i8080_daa(struct i8080 *const c)
{
	bool cy = c->cf;
	uint8_t correction = 0;

	const uint8_t lsb = c->r.eg8[REG_A] & 0x0F;
	const uint8_t msb = c->r.eg8[REG_A] >> 4;

	if (c->hf || lsb > 9)
		correction += 0x06;

	if (c->cf || msb > 9 || (msb >= 9 && lsb > 9)) {
		correction += 0x60;
		cy = 1;
	}
	i8080_add(c, &c->r.eg8[REG_A], correction, 0);
	c->cf = cy;
}

// switches the value of registers DE and HL
static inline void i8080_xchg(struct i8080 *const c)
{
	const uint16_t de = c->r.eg16[REG_DE];
	c->r.eg16[REG_DE] = c->r.eg16[REG_HL];
	c->r.eg16[REG_HL] = de;
}

// switches the value of a word at (sp) and HL
static inline void i8080_xthl(struct i8080 *const c)
{
	const uint16_t val = i8080_rw(c, c->r.eg16[REG_SP]);
	i8080_ww(c, c->r.eg16[REG_SP], c->r.eg16[REG_HL]);
	c->r.eg16[REG_HL] = val;
}

// executes one opcode
static inline void i8080_execute(struct i8080 *const c, uint8_t opcode)
{
	c->cyc += OPCODES_CYCLES[opcode];

	/*
	 * when DI is executed, interrupts won't be serviced
	 * until the end of next instruction:
	 */
	if (c->interrupt_delay > 0)
		c->interrupt_delay -= 1;

	/*
	 * Map the opcode to an instruction group.  Opcodes in the same
	 * instruction group can be processed with the same code.
	 */
	uint8_t ins = INSTRUCTION_TABLE[opcode];

	// XXX Restrict processing to a subset during the migration
	if (ins > LDAX_RR)
		goto do_opcode;
	switch (ins) {
	case MOV_R_R:
		*(c->reg8_table[DDD(opcode)]) = *(c->reg8_table[SSS(opcode)]);
		break;
	case MVI_R_N:
		*(c->reg8_table[DDD(opcode)]) = i8080_next_byte(c);
		break;
	case ADD_R:
		i8080_add(c, &c->r.eg8[REG_A],
			    *(c->reg8_table[SSS(opcode)]), 0);
		break;
	case ADC_R:
		i8080_add(c, &c->r.eg8[REG_A],
			    *(c->reg8_table[SSS(opcode)]), c->cf);
		break;
	case SUB_R:
		i8080_sub(c, &c->r.eg8[REG_A],
			    *(c->reg8_table[SSS(opcode)]), 0);
		break;
	case SBB_R:
		i8080_sub(c, &c->r.eg8[REG_A],
			    *(c->reg8_table[SSS(opcode)]), c->cf);
		break;
	case ANA_R:
		i8080_ana(c, *(c->reg8_table[SSS(opcode)]));
		break;
	case XRA_R:
		i8080_xra(c, *(c->reg8_table[SSS(opcode)]));
		break;
	case ORA_R:
		i8080_ora(c, *(c->reg8_table[SSS(opcode)]));
		break;
	case CMP_R:
		i8080_cmp(c, *(c->reg8_table[SSS(opcode)]));
		break;
	case INR_R:
		*(c->reg8_table[DDD(opcode)]) =
			i8080_inr(c, *(c->reg8_table[DDD(opcode)]));
		break;
	case DCR_R:
		*(c->reg8_table[DDD(opcode)]) =
			i8080_dcr(c, *(c->reg8_table[DDD(opcode)]));
		break;
	case RCC:
		i8080_cond_ret(c, condition(c, opcode));
		break;
	case JCC_NN:
		i8080_cond_jmp(c, condition(c, opcode));
		break;
	case CCC_NN:
		i8080_cond_call(c, condition(c, opcode));
		break;
	case RST_P:
		i8080_call(c, opcode & 0x38);
		break;
	case LXI_RR_NN:
		*(c->reg16_sp[RP(opcode)]) = i8080_next_word(c);
		break;
	case STAX_RR: // Only BC & DE
		i8080_wb(c, *(c->reg16_sp[RP(opcode)]), c->r.eg8[REG_A]);
		break;
	case INX_RR:
		*(c->reg16_sp[RP(opcode)]) += 1;
		break;
	case DAD_RR:
		i8080_dad(c, *(c->reg16_sp[RP(opcode)]));
		break;
	case LDAX_RR: // Only BC & DE
		c->r.eg8[REG_A] = i8080_rb(c, *(c->reg16_sp[RP(opcode)]));
		break;
	default:
		fprintf(stderr, "unhandled instruction %d (opcode %02x)\n",
			ins, opcode);
		exit(1);
	}
	return;

do_opcode:
	switch (opcode) {
		// 8 bit transfer instructions
	case 0x7E:
		c->r.eg8[REG_A] = i8080_rb(c, c->r.eg16[REG_HL]);
		break;		// MOV A,M

	case 0x3A:
		c->r.eg8[REG_A] = i8080_rb(c, i8080_next_word(c));
		break;		// LDA word

	case 0x46:
		c->r.eg8[REG_B] = i8080_rb(c, c->r.eg16[REG_HL]);
		break;		// MOV B,M

	case 0x4E:
		c->r.eg8[REG_C] = i8080_rb(c, c->r.eg16[REG_HL]);
		break;		// MOV C,M

	case 0x56:
		c->r.eg8[REG_D] = i8080_rb(c, c->r.eg16[REG_HL]);
		break;		// MOV D,M

	case 0x5E:
		c->r.eg8[REG_E] = i8080_rb(c, c->r.eg16[REG_HL]);
		break;		// MOV E,M

	case 0x66:
		c->r.eg8[REG_H] = i8080_rb(c, c->r.eg16[REG_HL]);
		break;		// MOV H,M

	case 0x6E:
		c->r.eg8[REG_L] = i8080_rb(c, c->r.eg16[REG_HL]);
		break;		// MOV L,M

	case 0x77:
		i8080_wb(c, c->r.eg16[REG_HL], c->r.eg8[REG_A]);
		break;		// MOV M,A
	case 0x70:
		i8080_wb(c, c->r.eg16[REG_HL], c->r.eg8[REG_B]);
		break;		// MOV M,B
	case 0x71:
		i8080_wb(c, c->r.eg16[REG_HL], c->r.eg8[REG_C]);
		break;		// MOV M,C
	case 0x72:
		i8080_wb(c, c->r.eg16[REG_HL], c->r.eg8[REG_D]);
		break;		// MOV M,D
	case 0x73:
		i8080_wb(c, c->r.eg16[REG_HL], c->r.eg8[REG_E]);
		break;		// MOV M,E
	case 0x74:
		i8080_wb(c, c->r.eg16[REG_HL], c->r.eg8[REG_H]);
		break;		// MOV M,H
	case 0x75:
		i8080_wb(c, c->r.eg16[REG_HL], c->r.eg8[REG_L]);
		break;		// MOV M,L

	case 0x36:
		i8080_wb(c, c->r.eg16[REG_HL], i8080_next_byte(c));
		break;		// MVI M,byte

	case 0x32:
		i8080_wb(c, i8080_next_word(c), c->r.eg8[REG_A]);
		break;		// STA word

		// 16 bit transfer instructions
	case 0x2A:
		c->r.eg16[REG_HL] = i8080_rw(c, i8080_next_word(c));
		break;		// LHLD
	case 0x22:
		i8080_ww(c, i8080_next_word(c), c->r.eg16[REG_HL]);
		break;		// SHLD
	case 0xF9:
		c->r.eg16[REG_SP] = c->r.eg16[REG_HL];
		break;		// SPHL

		// register exchange instructions
	case 0xEB:
		i8080_xchg(c);
		break;		// XCHG
	case 0xE3:
		i8080_xthl(c);
		break;		// XTHL

		// add byte instructions
	case 0x86:
		i8080_add(c, &c->r.eg8[REG_A],
			  i8080_rb(c, c->r.eg16[REG_HL]), 0);
		break;		// ADD M
	case 0xC6:
		i8080_add(c, &c->r.eg8[REG_A], i8080_next_byte(c), 0);
		break;		// ADI byte

		// add byte with carry-in instructions
	case 0x8E:
		i8080_add(c, &c->r.eg8[REG_A],
			  i8080_rb(c, c->r.eg16[REG_HL]), c->cf);
		break;		// ADC M
	case 0xCE:
		i8080_add(c, &c->r.eg8[REG_A], i8080_next_byte(c), c->cf);
		break;		// ACI byte

		// substract byte instructions
	case 0x96:
		i8080_sub(c, &c->r.eg8[REG_A],
			  i8080_rb(c, c->r.eg16[REG_HL]), 0);
		break;		// SUB M
	case 0xD6:
		i8080_sub(c, &c->r.eg8[REG_A], i8080_next_byte(c), 0);
		break;		// SUI byte

		// substract byte with borrow-in instructions
	case 0x9E:
		i8080_sub(c, &c->r.eg8[REG_A],
			  i8080_rb(c, c->r.eg16[REG_HL]), c->cf);
		break;		// SBB M
	case 0xDE:
		i8080_sub(c, &c->r.eg8[REG_A], i8080_next_byte(c), c->cf);
		break;		// SBI byte

		// control instructions
	case 0xF3:
		c->iff = 0;
		break;		// DI
	case 0xFB:
		c->iff = 1;
		c->interrupt_delay = 1;
		break;		// EI
	case 0x00:
		break;		// NOP
	case 0x76:
		c->halted = 1;
		break;		// HLT

		// increment byte instructions
	case 0x34:
		i8080_wb(c, c->r.eg16[REG_HL],
			 i8080_inr(c, i8080_rb(c, c->r.eg16[REG_HL])));
		break;		// INR M

		// decrement byte instructions
	case 0x35:
		i8080_wb(c, c->r.eg16[REG_HL],
			 i8080_dcr(c, i8080_rb(c, c->r.eg16[REG_HL])));
		break;		// DCR M

		// decrement register pair instructions
	case 0x0B:
		c->r.eg16[REG_BC] -= 1;
		break;		// DCX B
	case 0x1B:
		c->r.eg16[REG_DE] -= 1;
		break;		// DCX D
	case 0x2B:
		c->r.eg16[REG_HL] -= 1;
		break;		// DCX H
	case 0x3B:
		c->r.eg16[REG_SP] -= 1;
		break;		// DCX SP

		// special accumulator and flag instructions
	case 0x27:
		i8080_daa(c);
		break;		// DAA
	case 0x2F:
		c->r.eg8[REG_A] = ~c->r.eg8[REG_A];
		break;		// CMA
	case 0x37:
		c->cf = 1;
		break;		// STC
	case 0x3F:
		c->cf = !c->cf;
		break;		// CMC

		// rotate instructions
	case 0x07:
		i8080_rlc(c);
		break;		// RLC (rotate left)
	case 0x0F:
		i8080_rrc(c);
		break;		// RRC (rotate right)
	case 0x17:
		i8080_ral(c);
		break;		// RAL
	case 0x1F:
		i8080_rar(c);
		break;		// RAR

		// logical byte instructions
	case 0xA6:
		i8080_ana(c, i8080_rb(c, c->r.eg16[REG_HL]));
		break;		// ANA M
	case 0xE6:
		i8080_ana(c, i8080_next_byte(c));
		break;		// ANI byte

	case 0xAE:
		i8080_xra(c, i8080_rb(c, c->r.eg16[REG_HL]));
		break;		// XRA M
	case 0xEE:
		i8080_xra(c, i8080_next_byte(c));
		break;		// XRI byte

	case 0xB6:
		i8080_ora(c, i8080_rb(c, c->r.eg16[REG_HL]));
		break;		// ORA M
	case 0xF6:
		i8080_ora(c, i8080_next_byte(c));
		break;		// ORI byte

	case 0xBE:
		i8080_cmp(c, i8080_rb(c, c->r.eg16[REG_HL]));
		break;		// CMP M
	case 0xFE:
		i8080_cmp(c, i8080_next_byte(c));
		break;		// CPI byte

		// branch control/program counter load instructions
	case 0xC3:
		i8080_jmp(c, i8080_next_word(c));
		break;		// JMP

	case 0xE9:
		c->pc = c->r.eg16[REG_HL];
		break;		// PCHL
	case 0xCD:
		i8080_call(c, i8080_next_word(c));
		break;		// CALL

	case 0xC9:
		i8080_ret(c);
		break;		// RET

		// stack operation instructions
	case 0xC5:
		i8080_push_stack(c, c->r.eg16[REG_BC]);
		break;		// PUSH B
	case 0xD5:
		i8080_push_stack(c, c->r.eg16[REG_DE]);
		break;		// PUSH D
	case 0xE5:
		i8080_push_stack(c, c->r.eg16[REG_HL]);
		break;		// PUSH H
	case 0xF5:
		i8080_push_psw(c);
		break;		// PUSH PSW
	case 0xC1:
		c->r.eg16[REG_BC] = i8080_pop_stack(c);
		break;		// POP B
	case 0xD1:
		c->r.eg16[REG_DE] = i8080_pop_stack(c);
		break;		// POP D
	case 0xE1:
		c->r.eg16[REG_HL] = i8080_pop_stack(c);
		break;		// POP H
	case 0xF1:
		i8080_pop_psw(c);
		break;		// POP PSW

		// input/output instructions
	case 0xDB:		// IN
		c->r.eg8[REG_A] = c->port_in(c->userdata, i8080_next_byte(c));
		break;
	case 0xD3:		// OUT
		c->port_out(c->userdata, i8080_next_byte(c), c->r.eg8[REG_A]);
		break;

		// undocumented NOPs
	case 0x08:
	case 0x10:
	case 0x18:
	case 0x20:
	case 0x28:
	case 0x30:
	case 0x38:
		break;

		// undocumented RET
	case 0xD9:
		i8080_ret(c);
		break;

		// undocumented CALLs
	case 0xDD:
	case 0xED:
	case 0xFD:
		i8080_call(c, i8080_next_word(c));
		break;

		// undocumented JMP
	case 0xCB:
		i8080_jmp(c, i8080_next_word(c));
		break;
	default:
		fprintf(stderr, "unhandled opcode %02x\n", opcode);
		exit(1);
	}
}

// initialises the emulator with default values
void i8080_init(struct i8080 *const c)
{
	c->pc = 0;
	c->r.eg16[REG_SP] = 0;

	c->r.eg8[REG_A] = 0;
	c->r.eg8[REG_B] = 0;
	c->r.eg8[REG_C] = 0;
	c->r.eg8[REG_D] = 0;
	c->r.eg8[REG_E] = 0;
	c->r.eg8[REG_H] = 0;
	c->r.eg8[REG_L] = 0;

	c->sf = 0;
	c->zf = 0;
	c->hf = 0;
	c->pf = 0;
	c->cf = 0;
	c->iff = 0;

	c->cyc = 0;
	c->interrupt_pending = 0;
	c->interrupt_vector = 0;
	c->interrupt_delay = 0;
	c->halted = 0;

	c->userdata = NULL;
	c->read_byte = NULL;
	c->write_byte = NULL;
	c->port_in = NULL;
	c->port_out = NULL;

	c->reg8_table[0] = &c->r.eg8[REG_B];
	c->reg8_table[1] = &c->r.eg8[REG_C];
	c->reg8_table[2] = &c->r.eg8[REG_D];
	c->reg8_table[3] = &c->r.eg8[REG_E];
	c->reg8_table[4] = &c->r.eg8[REG_H];
	c->reg8_table[5] = &c->r.eg8[REG_L];
	c->reg8_table[6] = NULL;
	c->reg8_table[7] = &c->r.eg8[REG_A];

	c->reg16_sp[0] = &c->r.eg16[REG_BC];
	c->reg16_sp[1] = &c->r.eg16[REG_DE];
	c->reg16_sp[2] = &c->r.eg16[REG_HL];
	c->reg16_sp[3] = &c->r.eg16[REG_SP];
}

// executes one instruction
void i8080_step(struct i8080 *const c)
{
	/*
	 * interrupt processing: if an interrupt is pending and IFF is set,
	 * we execute the interrupt vector passed by the user.
	 */
	if (c->interrupt_pending && c->iff && c->interrupt_delay == 0) {
		c->interrupt_pending = 0;
		c->iff = 0;
		c->halted = 0;

		i8080_execute(c, c->interrupt_vector);
	} else if (!c->halted)
		i8080_execute(c, i8080_next_byte(c));
}

// asks for an interrupt to be serviced
void i8080_interrupt(struct i8080 *const c, uint8_t opcode)
{
	c->interrupt_pending = 1;
	c->interrupt_vector = opcode;
}

#ifdef I8080_DEBUG_OUTPUT

// simple disassemble function
static void disassemble_ins(struct i8080 *const c)
{
	static const char hex[16] = { '0', '1', '2', '3', '4', '5', '6', '7',
				      '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };
	char buf[16];
	const char *p;
	char *q;
	int chr, data;

	/*
	 * Copy the string representation of the instruction, substituting '#'
	 * for a single byte hex value from the next memory location and '$'
	 * for a double byte hex value from the next two memory locations.
	 */
	p = DISASSEMBLE_TABLE[i8080_rb(c, c->pc)];
	q = &buf[0];
	while ((chr = *p++)) {
		if (chr == '$')
			data = i8080_rb(c, c->pc + 1) +
			       (i8080_rb(c, c->pc + 2) << 8);
		else if (chr == '#') {
			data = i8080_rb(c, c->pc + 1);
			goto print2;
		}
		else {
			*q++ = chr;
			continue;
		}

		*q++ = hex[data >> 12 & 0xf];
		*q++ = hex[data >>  8 & 0xf];
print2:
		*q++ = hex[data >>  4 & 0xf];
		*q++ = hex[data >>  0 & 0xf];
		*q++ = 'h';
	}

	*q++ = 0;
	printf("%s", buf);
}

/*
 * outputs a debug trace of the emulator state to the standard output,
 * including registers and flags
 */

void i8080_debug_output(struct i8080 *const c)
{
	uint8_t f = 0;
	f |= c->sf << 7;
	f |= c->zf << 6;
	f |= c->hf << 4;
	f |= c->pf << 2;
	f |= 1 << 1;		// bit 1 is always 1
	f |= c->cf << 0;

	printf("PC:%04X AF:%04X BC:%04X DE:%04X HL:%04X SP:%04X T:%6lu "
	       "I:%c%c  ",
	       c->pc,
	       c->r.eg8[REG_A] << 8 | f,
	       c->r.eg16[REG_BC],
	       c->r.eg16[REG_DE],
	       c->r.eg16[REG_HL],
	       c->r.eg16[REG_SP],
	       c->cyc % 1000000,
	       c->iff ? 'E' : 'D',
	       c->interrupt_pending ? 'P' : ' ');
	disassemble_ins(c);
	printf("\n");
}
#endif // I8080_DEBUG_OUTPUT

#undef SET_ZSP
