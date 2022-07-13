use qoi;
use std::fs;

fn main() -> Result<(), std::io::Error> {
    let input_buffer = fs::read("input_no_terminator.qoi")?;
    let mut input_iter = input_buffer.iter();
    let header = qoi::parse_header(&mut input_iter).unwrap();
    dbg!(&header);
    let bitmap = qoi::decode(header, &mut input_iter).unwrap();
    assert_eq!(
        (header.width * header.height * (header.channels as u32)) as usize,
        bitmap.len(),
        "Didn't decode to the correct length"
    );
    fs::write("output.raw", &bitmap)?;
    dbg!(bitmap.len());
    let output = qoi::encode(header, &mut bitmap.iter()).unwrap();
    fs::write("output.qoi", output).expect("Unable to write file");
    Ok(())
}
