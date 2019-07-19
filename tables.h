// SPDX-License-Identifier: MIT

#ifndef I8080_TABLES_H_
#define I8080_TABLES_H_

#include <stdint.h>

#include "i8080.h"

/*
 * this array defines the number of cycles one opcode takes.
 * note that there are some special cases: conditional RETs and CALLs
 * add +6 cycles if the condition is met
 */
static const uint8_t OPCODES_CYCLES[] = {
//	 0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
	 4, 10,  7,  5,  5,  5,  7,  4,  4, 10,  7,  5,  5,  5,  7,  4,	// 0
	 4, 10,  7,  5,  5,  5,  7,  4,  4, 10,  7,  5,  5,  5,  7,  4,	// 1
	 4, 10, 16,  5,  5,  5,  7,  4,  4, 10, 16,  5,  5,  5,  7,  4,	// 2
	 4, 10, 13,  5, 10, 10, 10,  4,  4, 10, 13,  5,  5,  5,  7,  4,	// 3
	 5,  5,  5,  5,  5,  5,  7,  5,  5,  5,  5,  5,  5,  5,  7,  5,	// 4
	 5,  5,  5,  5,  5,  5,  7,  5,  5,  5,  5,  5,  5,  5,  7,  5,	// 5
	 5,  5,  5,  5,  5,  5,  7,  5,  5,  5,  5,  5,  5,  5,  7,  5,	// 6
	 7,  7,  7,  7,  7,  7,  7,  7,  5,  5,  5,  5,  5,  5,  7,  5,	// 7
	 4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,	// 8
	 4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,	// 9
	 4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,	// A
	 4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,	// B
	 5, 10, 10, 10, 11, 11,  7, 11,  5, 10, 10, 10, 11, 17,  7, 11,	// C
	 5, 10, 10, 10, 11, 11,  7, 11,  5, 10, 10, 10, 11, 17,  7, 11,	// D
	 5, 10, 10, 18, 11, 11,  7, 11,  5,  5, 10,  4, 11, 17,  7, 11,	// E
	 5, 10, 10,  4, 11, 11,  7, 11,  5,  5, 10,  4, 11, 17,  7, 11	// F
};

static const uint8_t INSTRUCTION_TABLE[256] = {
	/* 0x00 - 0x0F */
	NOP,
	LXI_RR_NN,
	STAX_RR,
	INX_RR,
	INR_R,
	DCR_R,
	MVI_R_N,
	RLC,

	NOP_UNDOCUMENTED,
	DAD_RR,
	LDAX_RR,
	DCX_RR,
	INR_R,
	DCR_R,
	MVI_R_N,
	RRC,

	/* 0x10 - 0x1F */
	NOP_UNDOCUMENTED,
	LXI_RR_NN,
	STAX_RR,
	INX_RR,
	INR_R,
	DCR_R,
	MVI_R_N,
	RAL,

	NOP_UNDOCUMENTED,
	DAD_RR,
	LDAX_RR,
	DCX_RR,
	INR_R,
	DCR_R,
	MVI_R_N,
	RAR,

	/* 0x20 - 0x2F */
	NOP_UNDOCUMENTED,
	LXI_RR_NN,
	SHLD_NN,
	INX_RR,
	INR_R,
	DCR_R,
	MVI_R_N,
	DAA,

	NOP_UNDOCUMENTED,
	DAD_RR,
	LHLD_NN,
	DCX_RR,
	INR_R,
	DCR_R,
	MVI_R_N,
	CMA,

	/* 0x30 - 0x3F */
	NOP_UNDOCUMENTED,
	LXI_RR_NN,
	STA_NN,
	INX_RR,
	INR_M,
	DCR_M,
	MVI_M_N,
	STC,

	NOP_UNDOCUMENTED,
	DAD_RR,
	LDA_NN,
	DCX_RR,
	INR_R,
	DCR_R,
	MVI_R_N,
	CMC,

	/* 0x40 - 0x4F */
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_M,
	MOV_R_R,

	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_M,
	MOV_R_R,

	/* 0x50 - 0x5F */
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_M,
	MOV_R_R,

	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_M,
	MOV_R_R,

	/* 0x60 - 0x6F */
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_M,
	MOV_R_R,

	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_M,
	MOV_R_R,

	/* 0x70 - 0x7F */
	MOV_M_R,
	MOV_M_R,
	MOV_M_R,
	MOV_M_R,
	MOV_M_R,
	MOV_M_R,
	HALT,
	MOV_M_R,

	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_R,
	MOV_R_M,
	MOV_R_R,

	/* 0x80 - 0x8F */
	ADD_R,
	ADD_R,
	ADD_R,
	ADD_R,
	ADD_R,
	ADD_R,
	ADD_M,
	ADD_R,

	ADC_R,
	ADC_R,
	ADC_R,
	ADC_R,
	ADC_R,
	ADC_R,
	ADC_M,
	ADC_R,

	/* 0x90 - 0x9F */
	SUB_R,
	SUB_R,
	SUB_R,
	SUB_R,
	SUB_R,
	SUB_R,
	SUB_M,
	SUB_R,

	SBB_R,
	SBB_R,
	SBB_R,
	SBB_R,
	SBB_R,
	SBB_R,
	SBB_M,
	SBB_R,

	/* 0xA0 - 0xAF */
	ANA_R,
	ANA_R,
	ANA_R,
	ANA_R,
	ANA_R,
	ANA_R,
	ANA_M,
	ANA_R,

	XRA_R,
	XRA_R,
	XRA_R,
	XRA_R,
	XRA_R,
	XRA_R,
	XRA_M,
	XRA_R,

	/* 0xB0 - 0xBF */
	ORA_R,
	ORA_R,
	ORA_R,
	ORA_R,
	ORA_R,
	ORA_R,
	ORA_M,
	ORA_R,

	CMP_R,
	CMP_R,
	CMP_R,
	CMP_R,
	CMP_R,
	CMP_R,
	CMP_M,
	CMP_R,

	/* 0xC0 - 0xCF */
	RCC,
	POP_RR,
	JCC_NN,
	JMP_NN,
	CCC_NN,
	PUSH_RR,
	ADI_N,
	RST_P,

	RCC,
	RET,
	JCC_NN,
	JMP_NN_UNDOCUMENTED,
	CCC_NN,
	CALL_NN,
	ACI_N,
	RST_P,

	/* 0xD0 - 0xDF */
	RCC,
	POP_RR,
	JCC_NN,
	OUT_N,
	CCC_NN,
	PUSH_RR,
	SUI_N,
	RST_P,

	RCC,
	RET_UNDOCUMENTED,
	JCC_NN,
	IN_N,
	CCC_NN,
	CALL_NN_UNDOCUMENTED,
	SBI_N,
	RST_P,

	/* 0xE0 - 0xEF */
	RCC,
	POP_RR,
	JCC_NN,
	XTHL,
	CCC_NN,
	PUSH_RR,
	ANI_N,
	RST_P,

	RCC,
	PCHL,
	JCC_NN,
	XCHG,
	CCC_NN,
	CALL_NN_UNDOCUMENTED,
	XRI_N,
	RST_P,

	/* 0xF0 - 0xFF */
	RCC,
	POP_RR,
	JCC_NN,
	DI,
	CCC_NN,
	PUSH_RR,
	ORI_N,
	RST_P,

	RCC,
	SPHL,
	JCC_NN,
	EI,
	CCC_NN,
	CALL_NN_UNDOCUMENTED,
	CPI_N,
	RST_P,

};

static const uint8_t  SZPR_FLAGS_TABLE[256] = {
    0x46, 0x02, 0x02, 0x06, 0x02, 0x06, 0x06, 0x02,
    0x02, 0x06, 0x06, 0x02, 0x06, 0x02, 0x02, 0x06,
    0x02, 0x06, 0x06, 0x02, 0x06, 0x02, 0x02, 0x06,
    0x06, 0x02, 0x02, 0x06, 0x02, 0x06, 0x06, 0x02,
    0x02, 0x06, 0x06, 0x02, 0x06, 0x02, 0x02, 0x06,
    0x06, 0x02, 0x02, 0x06, 0x02, 0x06, 0x06, 0x02,
    0x06, 0x02, 0x02, 0x06, 0x02, 0x06, 0x06, 0x02,
    0x02, 0x06, 0x06, 0x02, 0x06, 0x02, 0x02, 0x06,
    0x02, 0x06, 0x06, 0x02, 0x06, 0x02, 0x02, 0x06,
    0x06, 0x02, 0x02, 0x06, 0x02, 0x06, 0x06, 0x02,
    0x06, 0x02, 0x02, 0x06, 0x02, 0x06, 0x06, 0x02,
    0x02, 0x06, 0x06, 0x02, 0x06, 0x02, 0x02, 0x06,
    0x06, 0x02, 0x02, 0x06, 0x02, 0x06, 0x06, 0x02,
    0x02, 0x06, 0x06, 0x02, 0x06, 0x02, 0x02, 0x06,
    0x02, 0x06, 0x06, 0x02, 0x06, 0x02, 0x02, 0x06,
    0x06, 0x02, 0x02, 0x06, 0x02, 0x06, 0x06, 0x02,
    0x82, 0x86, 0x86, 0x82, 0x86, 0x82, 0x82, 0x86,
    0x86, 0x82, 0x82, 0x86, 0x82, 0x86, 0x86, 0x82,
    0x86, 0x82, 0x82, 0x86, 0x82, 0x86, 0x86, 0x82,
    0x82, 0x86, 0x86, 0x82, 0x86, 0x82, 0x82, 0x86,
    0x86, 0x82, 0x82, 0x86, 0x82, 0x86, 0x86, 0x82,
    0x82, 0x86, 0x86, 0x82, 0x86, 0x82, 0x82, 0x86,
    0x82, 0x86, 0x86, 0x82, 0x86, 0x82, 0x82, 0x86,
    0x86, 0x82, 0x82, 0x86, 0x82, 0x86, 0x86, 0x82,
    0x86, 0x82, 0x82, 0x86, 0x82, 0x86, 0x86, 0x82,
    0x82, 0x86, 0x86, 0x82, 0x86, 0x82, 0x82, 0x86,
    0x82, 0x86, 0x86, 0x82, 0x86, 0x82, 0x82, 0x86,
    0x86, 0x82, 0x82, 0x86, 0x82, 0x86, 0x86, 0x82,
    0x82, 0x86, 0x86, 0x82, 0x86, 0x82, 0x82, 0x86,
    0x86, 0x82, 0x82, 0x86, 0x82, 0x86, 0x86, 0x82,
    0x86, 0x82, 0x82, 0x86, 0x82, 0x86, 0x86, 0x82,
    0x82, 0x86, 0x86, 0x82, 0x86, 0x82, 0x82, 0x86
};

#ifdef I8080_DEBUG_OUTPUT
static const char* DISASSEMBLE_TABLE[] = {
	"nop", "lxi b,$", "stax b", "inx b", "inr b", "dcr b", "mvi b,#", "rlc",
	"*nop", "dad b", "ldax b", "dcx b", "inr c", "dcr c", "mvi c,#", "rrc",
	"*nop", "lxi d,$", "stax d", "inx d", "inr d", "dcr d", "mvi d,#",
	"ral", "*nop", "dad d", "ldax d", "dcx d", "inr e", "dcr e", "mvi e,#",
	"rar", "*nop", "lxi h,$", "shld", "inx h", "inr h", "dcr h", "mvi h,#",
	"daa", "*nop", "dad h", "lhld", "dcx h", "inr l", "dcr l", "mvi l,#",
	"cma", "*nop", "lxi sp,$","sta $", "inx sp", "inr m", "dcr m",
	"mvi m,#", "stc", "*nop", "dad sp", "lda $", "dcx sp", "inr a", "dcr a",
	"mvi a,#", "cmc", "mov b,b", "mov b,c", "mov b,d", "mov b,e", "mov b,h",
	"mov b,l", "mov b,m", "mov b,a", "mov c,b", "mov c,c", "mov c,d",
	"mov c,e", "mov c,h", "mov c,l", "mov c,m", "mov c,a", "mov d,b",
	"mov d,c", "mov d,d", "mov d,e", "mov d,h", "mov d,l", "mov d,m",
	"mov d,a", "mov e,b", "mov e,c", "mov e,d", "mov e,e", "mov e,h",
	"mov e,l", "mov e,m", "mov e,a", "mov h,b", "mov h,c", "mov h,d",
	"mov h,e", "mov h,h", "mov h,l", "mov h,m", "mov h,a", "mov l,b",
	"mov l,c", "mov l,d", "mov l,e", "mov l,h", "mov l,l", "mov l,m",
	"mov l,a", "mov m,b", "mov m,c", "mov m,d", "mov m,e", "mov m,h",
	"mov m,l", "hlt", "mov m,a", "mov a,b", "mov a,c", "mov a,d", "mov a,e",
	"mov a,h", "mov a,l", "mov a,m", "mov a,a", "add b", "add c", "add d",
	"add e", "add h", "add l", "add m", "add a", "adc b", "adc c", "adc d",
	"adc e", "adc h", "adc l", "adc m", "adc a", "sub b", "sub c", "sub d",
	"sub e", "sub h", "sub l", "sub m", "sub a", "sbb b", "sbb c", "sbb d",
	"sbb e", "sbb h", "sbb l", "sbb m", "sbb a", "ana b", "ana c", "ana d",
	"ana e", "ana h", "ana l", "ana m", "ana a", "xra b", "xra c", "xra d",
	"xra e", "xra h", "xra l", "xra m", "xra a", "ora b", "ora c", "ora d",
	"ora e", "ora h", "ora l", "ora m", "ora a", "cmp b", "cmp c", "cmp d",
	"cmp e", "cmp h", "cmp l", "cmp m", "cmp a", "rnz", "pop b", "jnz $",
	"jmp $", "cnz $", "push b", "adi #", "rst 0", "rz", "ret", "jz $",
	"*nop", "cz $", "call $", "aci #", "rst 1", "rnc", "pop d", "jnc $",
	"out p", "cnc $", "push d", "sui #", "rst 2", "rc", "*ret", "jc $",
	"in p", "cc $", "*call", "sbi #", "rst 3", "rpo", "pop h", "jpo $",
	"xthl", "cpo $", "push h", "ani #", "rst 4", "rpe", "pchl", "jpe $",
	"xchg", "cpe $", "*call", "xri #", "rst 5", "rp", "pop psw", "jp $",
	"di", "cp $", "push psw", "ori #", "rst 6", "rm", "sphl", "jm $", "ei",
	"cm $", "*call $", "cpi #", "rst 7"
};
#endif // I8080_DEBUG_OUTPUT

#endif // I8080_TABLES_H_
