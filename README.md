# AVR emulator

[![Build Status](https://travis-ci.org/dylanmckay/avr.svg?branch=master)](https://travis-ci.org/dylanmckay/avr)
[![Crates.io](https://img.shields.io/crates/v/avr.svg)](https://crates.io/crates/avr)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

This program emulates an 8-bit AVR microcontroller. It supports the trivial
C "Hello World!" program.

_NOTE_: This emulator isn't quite complete. One notable thing is that not all status register
updates are implemented for all instructions (#2).

Given some C++ source.

```cpp
#include <avr/io.h>
#include <util/delay.h>

int main() {
  DDRB |= _BV(PB6);

  for(uint8_t i=0; i<5; i++) {
    PORTB |= _BV(PB6);
    _delay_ms(500);

    PORTB &= ~_BV(PB6);
    _delay_ms(500);
  }

  return 0;
}
```

```bash
# Generate an ELF object file for the Atmega328p
avr-g++ hello_world.c -DF_CPU=8000000 -mmcu=atmega328p -O2 -o hello_world.o

# Generate a raw binary
avr-objcopy -I elf32-avr -O binary hello_world.o hello_world.bin

cargo run hello_world.bin
```
