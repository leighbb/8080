#ifndef I8080_I8080_H_
#define I8080_I8080_H_

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

typedef struct i8080 {
    uint16_t pc, sp; // program counter, stack pointer
    uint8_t a, b, c, d, e, h, l; // registers
    bool sf, zf, hf, pf, cf, iff; // flags: sign, zero, half-carry, parity,
                                  // carry, interrupt flip-flop

    unsigned long cyc; // cycle count
    bool halted;
    bool interrupt_pending;
    uint8_t interrupt_vector;
    uint8_t interrupt_delay;

    // memory + io interface
    void* userdata; // general purpose pointer for the user
    uint8_t (*read_byte)(void*, uint16_t);
    void (*write_byte)(void*, uint16_t, uint8_t);
    uint8_t (*port_in)(void*, uint8_t);
    void (*port_out)(void*, uint8_t, uint8_t);
} i8080;

void i8080_init(i8080* const c);
void i8080_step(i8080* const c);
void i8080_interrupt(i8080* const c, uint8_t opcode);
void i8080_debug_output(i8080* const c);

#endif  // I8080_I8080_H_
