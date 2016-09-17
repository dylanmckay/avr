
# AVR emulator

This program emulates an 8-bit AVR microcontroller. It supports the trivial
C "Hello World!" program.

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
