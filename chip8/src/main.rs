use std::fs::File;
use std::env;
use std::io::Read;

fn main() {
    let mut arguments = env::args();
    println!("{:?}", arguments);

    let mut file = File::open(arguments.next().unwrap())
        .expect("Failed opening file (Does the file exist?)");
    
    let mut data = Vec::new();
    file.read_to_end(&mut data);

    println!("{:?}", data);
    
}
