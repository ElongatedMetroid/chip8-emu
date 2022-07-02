use std::{fs::OpenOptions, io::Read, process};

use chip8::Chip8;
use clap::{Command, Arg};
use rand::RngCore;

extern crate chip8;

fn main() {
    let (path, scale) = parse_args();

    // initialize hardware
    let mut chip8 = Chip8::new(rand::random());

    // handle the rom, open it and load it
    handle_rom(&path, &mut chip8);
    run(scale, &mut chip8);
}

fn run<R: RngCore>(scale: u32, chip8: &mut Chip8<R>) {
    // setup graphics
    // setup input

    loop {
        // emulate cycles

        // draw image/update screen

        // get input
    }
}

fn handle_rom<R: RngCore>(path: &str, chip8: &mut Chip8<R>) {
    let mut rom = Vec::new();

    OpenOptions::new()
        .read(true)
        .open(path)
        .expect("ROM does not exist")
        .read_to_end(&mut rom)
        .expect("Could not read ROM");

    match chip8.load_rom(&rom) {
        Ok(_) => {
            println!("Loaded ROM: Success");
            println!("Memory: {:?}", chip8.mem);
        },
        Err(e) => {
            eprintln!("There was a problem loading the ROM :(. {:?}", e);
            process::exit(1);
        }
    }
}

fn parse_args() -> (String, u32) {
    let app = Command::new("Chip8")
        .version("0.1.0")
        .author("Nate")
        .arg(
            Arg::new("scale")
                .short('s')
                .long("scale")
                .value_name("N")
                .help("Controls the scaling factor")
                .takes_value(true)
                .default_value("8")
                .validator(|scale| match scale.parse::<u32>() {
                    Ok(_) => Ok(()),
                    Err(e) => Err(format!("{}", e)),
                }),
        )
        .arg(
            Arg::new("path")
                .help("Path to the CHIP-8 ROM file")
                .index(1)
                .required(true)
        )
        .get_matches();

    let scale = app
        .value_of("scale")
        .map(|s| s.parse::<u32>().expect("Scale factor cannot be parsed"))
        .expect("Scale argument is not defined");

    let path = app
        .value_of("path")
        .expect("Path argument is not defined");

    (path.to_string(), scale)
}