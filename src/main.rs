use qoi::PixelBuffer;
use std::fs;

fn main() -> Result<(), std::io::Error> {
    let input_buffer = fs::read("input.qoi")?;
    let (header, bitmap) = qoi::decode(input_buffer.into_iter()).unwrap();
    assert_eq!(
        (header.width * header.height) as usize,
        bitmap.pixel_count(),
        "Didn't decode to the correct length"
    );
    match &bitmap {
        PixelBuffer::RGBA(vec) => fs::write("output.rgba", vec)?,
        PixelBuffer::RGB(vec) => fs::write("output.rgb", vec)?,
    };
    let output = qoi::encode(header, bitmap).unwrap();
    fs::write("output.qoi", output).expect("Unable to write file");
    Ok(())
}
