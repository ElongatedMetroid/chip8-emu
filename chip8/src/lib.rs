use std::ops::{IndexMut, Index};

use rand::{RngCore, SeedableRng};
use rand::rngs::SmallRng;

const ROM_ADDR: usize = 0x200;
const MEM_SIZE: usize =  0x1000;
pub const SCREEN_WIDTH: usize = 64;
pub const SCREEN_HEIGTH: usize = 32;
const FRAME_TIME: isize = 16666;

/// Address of SPRITE_CHARS
const SPRITE_CHARS_ADDR: u16 = 0x0000;
/// Character sprite data in hexadeimal
const SPRITE_CHARS: [[u8; 5]; 0x10] = [
    [0xF0, 0x90, 0x90, 0x90, 0xF0], // 0
    [0x20, 0x60, 0x20, 0x20, 0x70], // 1
    [0xF0, 0x10, 0xF0, 0x80, 0xF0], // 2
    [0xF0, 0x10, 0xF0, 0x10, 0xF0], // 3
    [0x90, 0x90, 0xF0, 0x10, 0x10], // 4
    [0xF0, 0x80, 0xF0, 0x10, 0xF0], // 5
    [0xF0, 0x80, 0xF0, 0x90, 0xF0], // 6
    [0xF0, 0x10, 0x20, 0x40, 0x40], // 7
    [0xF0, 0x90, 0xF0, 0x90, 0xF0], // 8
    [0xF0, 0x90, 0xF0, 0x10, 0xF0], // 9
    [0xF0, 0x90, 0xF0, 0x90, 0x90], // A
    [0xE0, 0x90, 0xE0, 0x90, 0xE0], // B
    [0xF0, 0x80, 0x80, 0x80, 0xF0], // C
    [0xE0, 0x90, 0x90, 0x90, 0xE0], // D
    [0xF0, 0x80, 0xF0, 0x80, 0xF0], // E
    [0xF0, 0x80, 0xF0, 0x80, 0x80], // F
];                         

#[derive(Debug)]
pub enum Error {
    InvalidOp(u8, u8),
    RomTooBig(usize),
    PcOutOfBounds(u16),
    Debug,
}

#[derive(Clone, Copy)]
struct Reg(u8);
#[derive(Debug)]
struct Regs([u8; 0x10]);

impl Regs {
    fn new() -> Self {
        Self([0; 0x10])
    }
}

impl Index<Reg> for Regs {
    type Output = u8;

    fn index(&self, reg: Reg) -> &Self::Output {
        &self.0[reg.0 as usize]
    }
}

impl IndexMut<Reg> for Regs {
    fn index_mut(&mut self, reg: Reg) -> &mut Self::Output {
        &mut self.0[reg.0 as usize]
    }
}

/// Chip8 Struct
/// This struct contains all the hardware components of the CHIP-8
#[derive(Debug)]
pub struct Chip8<R: RngCore> {
    pub mem: [u8; MEM_SIZE],
    v: Regs,                                    // Register Set
    i: u16,                                     // Index Register
    pc: u16,                                    // Program Counter
    stack: [u16; 0x10],                         // Stack
    sp: u8,                                     // Stack Pointer
    dt: u8,                                     // Delay Timer
    st: u8,                                     // Sound Timer
    keypad: u16,                                // Keypad
    fb: [u8; SCREEN_WIDTH * SCREEN_HEIGTH / 8], // Framebuffer
    tone: bool,                                 // Tone output enable
    time: isize,                                // Overtime in microseconds
    rng: R,                                     // Instance of a random number generator
}

impl Chip8<SmallRng> {
    pub fn new(seed: u64) -> Self {
        let mut mem = [0; MEM_SIZE];
        // load character sprites into memory
        for (i, sprite) in SPRITE_CHARS.iter().enumerate() {
            let p = SPRITE_CHARS_ADDR as usize + i * sprite.len();
            mem[p..p + sprite.len()].copy_from_slice(sprite)
        }
        Self {
            mem,
            v: Regs::new(),
            i: 0,
            pc: ROM_ADDR as u16,
            stack: [0; 0x10],
            sp: 0,
            dt: 0,
            st: 0,
            keypad: 0,
            fb: [0; SCREEN_WIDTH * SCREEN_HEIGTH / 8],
            tone: false,
            time: 0,
            rng: rand::rngs::SmallRng::seed_from_u64(seed),
        }
    }
}

/// Get the lowest 12 bits of the instruction
/// These 12 bits should be used as an address
/// 
/// # Examples
/// ```
/// 0x10 => self.op_jp(nnn!(w0, w1))
/// 
/// fn op_jp(&mut self, addr: u16) -> usize {
///     self.pc = addr;
///     // snip ...
/// }
/// ```
macro_rules! nnn {
    ($w0:expr, $w1:expr) => {
        //  1111 1111
        // &0000 1111
        //  0000 0000 0000 1111
        //  0000 1111 0000 0000
        // |0000 0000 1111 1111
        //  0000 1111 1111 1111
        (($w0 & 0x0f) as u16) << 8 | $w1 as u16
    };
}

pub fn lo_nib(b: u8) -> u8 {
    //  1111 1111
    // &0000 1111
    //  0000 1111
    b & 0x0f
}

pub fn hi_nib(b: u8) -> u8 {
    //  1111 1111
    // &1111 0000
    //  1111 0000
    //  0000 1111
    (b >> 4) & 0x0f
}

// TODO: Create cachers for some values
impl<R: RngCore> Chip8<R> {
    pub fn tone(&self) -> bool {
        self.tone
    }

    pub fn fb(&self) -> [u8; SCREEN_WIDTH * SCREEN_HEIGTH / 8] {
        self.fb
    }

    pub fn load_rom(&mut self, rom: &[u8]) -> Result<(), Error> {
        if rom.len() > MEM_SIZE - ROM_ADDR {
            return Err(Error::RomTooBig(rom.len()));
        }
        // Copy rom into memory to location 0x200 to 0x200 + the rom size
        self.mem[ROM_ADDR..ROM_ADDR + rom.len()].copy_from_slice(rom);
        Ok(())
    }

    /// This funtions executes instructions and simulates hardware for the duartion of a frame
    pub fn frame(&mut self, keypad: u16) -> Result<(), Error> {
        // set the keypad to the inputs that were pushed
        self.keypad = keypad;
        // if the delay timer is not 0 
        if self.dt != 0 {
            // subtract 1 from it
            self.dt -= 1;
        }
        // set the tone equal to true if the sound timer does not equal 0
        self.tone = if self.st != 0 {
            // subtract 1
            self.st -= 1;
            // return true to the tone
            true
        } else {
            // set tone to false
            false
        };
        // add one frame to time
        self.time += FRAME_TIME;

        // while the time is greater than 0
        // in other words go until it has been a frame
        while self.time > 0 {
            // If the program counter has gone out of bounds
            if self.pc as usize > MEM_SIZE - 1 {
                // Return an error stating the PC went out of bounds
                return Err(Error::PcOutOfBounds(self.pc));
            }
            // fetch byte one of the instuction
            let w0 = self.mem[self.pc as usize];
            // fetch byte two of the instruction
            let w1 = self.mem[self.pc as usize + 1];
            // get the elapsed time of the step
            let elapsed_time = self.step(w0, w1)?;
            // subtract elapsed time from the instruction from the time
            self.time -= elapsed_time as isize;
        }
        Ok(())
    }

    // w0: 
    fn step(&mut self, w0: u8, w1: u8) -> Result<usize, Error> {
        Ok(match w0 & 0xf0 {
            0x00 => match w1 {
                0xe0 => self.op_cls(),
                0xee => self.op_ret(),
                _ => self.op_call_rca_1802(nnn!(w0, w1)),
            },
            0x10 => self.op_jp(nnn!(w0, w1)),
            0x20 => self.op_call(nnn!(w0, w1)),
            0x30 => self.op_se(self.v[Reg(lo_nib(w0))], w1),
            0x40 => self.op_sne(self.v[Reg(lo_nib(w0))], w1),
            0x50 => self.op_se(self.v[Reg(lo_nib(w0))], self.v[Reg(hi_nib(w1))]),
            0x60 => self.op_ld(Reg(lo_nib(w0)), w1),
            0x70 => self.op_add(Reg(lo_nib(w0)), w1),
            0x80 => {
                let x = Reg(lo_nib(w0));
                let y = self.v[Reg(hi_nib(w1))];

                match lo_nib(w1) {
                    0x00 => self.op_ld(x, y),
                    0x01 => self.op_or(x, y),
                    0x02 => self.op_and(x, y),
                    0x03 => self.op_xor(x, y),
                    0x04 => self.op_add(x, y),
                    0x05 => self.op_sub(x, y),
                    0x06 => self.op_shr(x),
                    0x07 => self.op_subn(x, y),
                    0x0E => self.op_shl(x),
                    _ => return Err(Error::InvalidOp(w0, w1)),
                }
            },
            0x90 => self.op_sne(self.v[Reg(lo_nib(w0))], self.v[Reg(hi_nib(w1))]),
            0xA0 => self.op_ld_i(nnn!(w0, w1)),
            0xB0 => self.op_jp(nnn!(w0, w1)),
            0xC0 => self.op_rnd(Reg(lo_nib(w0)), w1),
            0xD0 => self.op_drw(self.v[Reg(lo_nib(w0))], self.v[Reg(hi_nib(w1))], lo_nib(w1)),
            0xE0 => {
                match w1 {
                    0x9E => self.op_skp(self.v[Reg(lo_nib(w0))]),
                    0xA1 => self.op_sknp(self.v[Reg(lo_nib(w0))]),
                    _ => return Err(Error::InvalidOp(w0, w1)),
                }
            },
            0xF0 => {
                match w1 {
                    0x07 => self.op_ld(Reg(lo_nib(w0)), self.dt),
                    0x0A => self.op_ld_vx_k(Reg(lo_nib(w0))),
                    0x15 => self.op_ld_dt(self.v[Reg(lo_nib(w0))]),
                    0x18 => self.op_ld_st(self.v[Reg(lo_nib(w0))]),
                    0x1E => self.op_add16(self.v[Reg(lo_nib(w0))]),
                    0x29 => self.op_ld_f(self.v[Reg(lo_nib(w0))]),
                    0x33 => self.op_ld_b(self.v[Reg(lo_nib(w0))]),
                    0x55 => self.op_ld_i_vx(self.v[Reg(lo_nib(w0))]),
                    0x65 => self.op_ld_vx_i(self.v[Reg(lo_nib(w0))]),
                    _ => return Err(Error::InvalidOp(w0, w1)),
                }
            }

            _ => return Err(Error::InvalidOp(w0, w1))
        })
    }

    /// Clear the display
    fn op_cls(&mut self) -> usize {
        // zero out all data in frame buffer
        for b in self.fb.iter_mut() {
            *b = 0
        }
        // increment program counter
        self.pc += 2;
        109
    }
    fn op_call_rca_1802(&mut self, _addr: u16) -> usize {
        100
    }
    /// Op: Return from a subroutine.
    fn op_ret(&mut self) -> usize {
        // decrement stack pointer
        self.sp -= 1;
        // set the program counter to the data on the top of the stack
        self.pc = self.stack[self.sp as usize];
        105
    }
    /// Op: Jump to addr.
    fn op_jp(&mut self, addr: u16) -> usize {
        // set the program counter to the address
        self.pc = addr;
        105
    }
    /// Op: Call subroutine at addr.
    fn op_call(&mut self, addr: u16) -> usize {
        // set the top of the stack to the program counter + 2
        self.stack[self.sp as usize] = self.pc + 2;
        // increment stack pointer
        self.sp += 1;
        // set the program counter to the address
        self.pc = addr;
        105
    }
    /// Op: Skip next instruction if a == b.
    fn op_se(&mut self, a: u8, b: u8) -> usize {
        // if a is equal to b...
        if a == b {
            // skip the next instruction
            self.pc += 4;
        } else {
            // otherwise go to the next instruction
            self.pc += 2;
        }
        61
    }
    /// Op: Skip next instruction if a != b.
    fn op_sne(&mut self, a: u8, b: u8) -> usize {
        if a != b {
            self.pc += 4;
        } else {
            self.pc += 2;
        }
        61
    }
    /// Op: Set Vx = v.
    fn op_ld(&mut self, x: Reg, v: u8) -> usize {
        self.v[x] = v;
        self.pc += 2;
        27
    }
    /// Op: Wait for a key press, store the value of the key in Vx.
    fn op_ld_vx_k(&mut self, x: Reg) -> usize {
        for i in 0..0x10 {
            // if there was a keypress on any button
            if 1 << i & self.keypad != 0 {
                // store the value of the key in Vx
                self.v[x] = i as u8;
                // increment the program counter
                self.pc += 2;
                break;
            }
        }
        200
    }
    /// Op: Set delay timer = Vx.
    fn op_ld_dt(&mut self, v: u8) -> usize {
        // set the delay timer
        self.dt = v;
        // go to next instruction
        self.pc += 2;
        45
    }
    /// Op: Set sound timer = Vx.
    fn op_ld_st(&mut self, v: u8) -> usize {
        self.st = v;
        self.pc += 2;
        45
    }
    /// Op: Set I = location of sprite for digit v.
    fn op_ld_f(&mut self, v: u8) -> usize {
        // set I to the character sprite for v
        self.i = SPRITE_CHARS_ADDR + v as u16 * 5;
        self.pc += 2;
        91
    }
    /// Op: Store BCD representation of v in memory locations I, I+1, and I+2.
    fn op_ld_b(&mut self, v: u8) -> usize {
        let d2 = v / 100;
        let v = v - d2 * 100;
        let d1 = v / 10;
        let v = v - d1 * 10;
        let d0 = v / 1;
        self.mem[self.i as usize + 0] = d2;
        self.mem[self.i as usize + 1] = d1;
        self.mem[self.i as usize + 2] = d0;
        self.pc += 2;
        927
    }
    /// Op: Store registers V0 through Vx in memory starting at location I.
    fn op_ld_i_vx(&mut self, x: u8) -> usize {
        for i in 0..x + 1 {
            self.mem[self.i as usize + i as usize] = self.v[Reg(i)];
        }
        self.pc += 2;
        605
    }
    /// Op: Read registers V0 through Vx from memory starting at location I.
    fn op_ld_vx_i(&mut self, x: u8) -> usize {
        for i in 0..x + 1 {
            self.v[Reg(i)] = self.mem[self.i as usize + i as usize];
        }
        self.pc += 2;
        605
    }
    /// Set Vx = Vx + b
    fn op_add(&mut self, x: Reg, b: u8) -> usize {
        // res = Vx + b, overflow = true if Vx + b overflows, otherwise its false
        let (res, overflow) = self.v[x].overflowing_add(b);
        // set register Vx to Vx + b
        self.v[x] = res;
        // set register Vf to true if we overflowed, in addition the Vf register
        // is the carry flag.
        self.v[Reg(0xf)] = if overflow { 1 } else { 0 };
        // increment program counter by 2 since all CHIP-8 instructions are 2 bytes each
        self.pc += 2;
        // return 45 since 45 microseconds were elapsed
        45
    }
    /// Op: Set I = I + b.
    fn op_add16(&mut self, b: u8) -> usize {
        self.i += b as u16;
        self.pc += 2;
        86
    }
    /// Op: Set Vx = Vx OR b.
    fn op_or(&mut self, x: Reg, b: u8) -> usize {
        self.v[x] |= b;
        self.pc += 2;
        200
    }
    /// Op: Set Vx = Vx AND b.
    fn op_and(&mut self, x: Reg, b: u8) -> usize {
        self.v[x] &= b;
        self.pc += 2;
        200
    }
    /// Op: Set Vx = Vx XOR b.
    fn op_xor(&mut self, x: Reg, b: u8) -> usize {
        self.v[x] ^= b;
        self.pc += 2;
        200
    }
    /// Op: Set Vx = Vx - b.
    fn op_sub(&mut self, x: Reg, b: u8) -> usize {
        let (res, overflow) = self.v[x].overflowing_sub(b);
        self.v[x] = res;
        self.v[Reg(0xf)] = if overflow { 0 } else { 1 };
        self.pc += 2;
        200
    }
    /// Op: Set Vx = b - Vx, set Vf = NOT borrow.
    fn op_subn(&mut self, x: Reg, b: u8) -> usize {
        let (res, overflow) = b.overflowing_sub(self.v[x]);
        self.v[x] = res;
        self.v[Reg(0xf)] = if overflow { 0 } else { 1 };
        self.pc += 2;
        200
    }
    /// Op: Set Vx = Vx >> 1.
    fn op_shr(&mut self, x: Reg) -> usize {
        self.v[Reg(0xf)] = self.v[x] & 0b00000001;
        let (res, _) = self.v[x].overflowing_shr(1);
        self.v[x] = res;
        self.pc += 2;
        200
    }
    /// Op: Set Vx = Vx << 1.
    fn op_shl(&mut self, x: Reg) -> usize {
        self.v[Reg(0xf)] = (self.v[x] & 0b10000000) >> 7;
        let (res, _) = self.v[x].overflowing_shl(1);
        self.v[x] = res;
        self.pc += 2;
        200
    }
    /// Op: Set I = addr
    fn op_ld_i(&mut self, addr: u16) -> usize {
        self.i = addr;
        self.pc += 2;
        55
    }
    /// Op: Set Vx = random byte AND v
    fn op_rnd(&mut self, x: Reg, v: u8) -> usize {
        self.v[x] = (self.rng.next_u32() as u8) & v;
        self.pc += 2;
        164
    }
    /// Op: Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.
    fn op_drw(&mut self, pos_x: u8, pos_y: u8, n: u8) -> usize {
        let pos_x = pos_x % 64;
        let pos_y = pos_y % 32;
        let fb = &mut self.fb;
        let shift = pos_x % 8;
        let col_a = pos_x as usize / 8;
        let col_b = (col_a + 1) % (SCREEN_WIDTH / 8);
        let mut collision = 0;
        for i in 0..(n as usize) {
            let byte = self.mem[self.i as usize + i];
            let y = (pos_y as usize + i) % SCREEN_HEIGTH;
            let a = byte >> shift;
            let fb_a = &mut fb[y * SCREEN_WIDTH / 8 + col_a];
            collision |= *fb_a & a;
            *fb_a ^= a;
            if shift != 0 {
                let b = byte << (8 - shift);
                let fb_b = &mut fb[y * SCREEN_WIDTH / 8 + col_b];
                collision |= *fb_b & b;
                *fb_b ^= b;
            }
        }
        self.v[Reg(0xf)] = if collision != 0 { 1 } else { 0 };
        self.pc += 2;
        22734
    }
    /// Op: Skip next instruction if key with the value of v is pressed.
    fn op_skp(&mut self, v: u8) -> usize {
        if 1 << v & self.keypad != 0 {
            self.pc += 4;
        } else {
            self.pc += 2;
        }
        73
    }
    /// Op: Skip next instruction if key with the value of v is not pressed.
    fn op_sknp(&mut self, v: u8) -> usize {
        if 1 << v & self.keypad == 0 {
            self.pc += 4;
        } else {
            self.pc += 2;
        }
        73
    }
}