use std::ops::{IndexMut, Index};

use rand::{RngCore, SeedableRng};
use rand::rngs::SmallRng;

const ROM_ADDR: usize = 0x200;
const MEM_SIZE: usize =  0x1000;
pub const SCREEN_WIDTH: usize = 64;
pub const SCREEN_HEIGTH: usize = 32;

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
    (b & 0xf0) >> 4
}

impl<R: RngCore> Chip8<R> {
    pub fn load_rom(&mut self, rom: &[u8]) -> Result<(), Error> {
        if rom.len() > MEM_SIZE - ROM_ADDR {
            return Err(Error::RomTooBig(rom.len()));
        }
        // Copy rom into memory to location 0x200 to 0x200 + the rom size
        self.mem[ROM_ADDR..ROM_ADDR + rom.len()].copy_from_slice(rom);
        Ok(())
    }

    // w0: 
    // fn step(&mut self, w0: u8, w1: u8) -> Result<usize, Error> {
    //     Ok(match w0 & 0xf0 {
    //         0x00 => match w1 {
    //             0xe0 => self.op_cls(),
    //             0xee => self.op_ret(),
    //             _ => self.op_call_rca_1802(nnn!(w0, w1)),
    //         },
    //         0x10 => self.op_jp(nnn!(w0, w1)),
    //         0x20 => self.op_call(nnn!(w0, w1)),
    //         0x30 => self.op_se(self.v[Reg(lo_nib(w0))], w1),
    //     })
    // }

    

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
}