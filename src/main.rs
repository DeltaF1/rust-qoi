use std::fs;

struct Header {
    width: u32,
    height: u32,
    channels: Channels,
    colorspace: ColorSpace,
}

enum Channels {
    RGB,
    RGBA,
}

#[allow(non_camel_case_types)]
enum ColorSpace {
    sRGB,
    Linear,
}

#[repr(packed)]
#[derive(Copy, Clone)]
struct RGBA {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

type u6 = u8;
type u2 = u8;
type Index = usize;

fn get_u2(byte: u8, offset: u8) -> u2 {
    (byte >> offset) & 0b11
}

fn hash(pixel: RGBA) -> Index {
    ((pixel.r.wrapping_mul(3)).wrapping_add(
        (pixel.g.wrapping_mul(5))
            .wrapping_add((pixel.b.wrapping_mul(7)).wrapping_add(pixel.a.wrapping_mul(11))),
    ) % 64)
        .into()
}

enum Op {
    Index { index: Index },
    Diff { dr: u2, dg: u2, db: u2 },
    Luma { dg: u6 },
    Run { run: u6 },
    RGB,
    RGBA,
}

impl From<u8> for Op {
    fn from(byte: u8) -> Op {
        let tag = byte >> 6;
        match tag {
            0b00 => Op::Index {
                index: (byte & 0b00111111) as usize,
            },
            0b01 => Op::Diff {
                dr: get_u2(byte, 4),
                dg: get_u2(byte, 2),
                db: get_u2(byte, 0),
            },
            0b10 => Op::Luma {
                dg: (byte & 0b00111111),
            },
            0b11 => match byte {
                0xff => Op::RGBA,
                0xfe => Op::RGB,
                _ => Op::Run {
                    run: byte & 0b00111111,
                },
            },
            // Unreachable since u8 >> 6 is only 2 bits
            4_u8..=u8::MAX => unreachable!(),
        }
    }
}

#[derive(Debug)]
enum QOIError {
    EndOfStream,
}

fn push_rgba(vec: &mut Vec<u8>, pixel: RGBA) {
    vec.push(pixel.r);
    vec.push(pixel.g);
    vec.push(pixel.b);
    vec.push(pixel.a);
}

// Change this to an iterator API
fn decode(qoi: &[u8]) -> Result<Vec<u8>, QOIError> {
    let mut previous = RGBA {
        r: 0,
        g: 0,
        b: 0,
        a: 255,
    };
    let mut lookup_table = [RGBA {
        r: 0,
        g: 0,
        b: 0,
        a: 0,
    }; 64];
    let mut output = vec![];

    let mut iter = qoi
        .iter()
        .skip(14); // Skip over the header for now

    while let Some(qoi_byte) = iter.next() {
        let op: Op = (*qoi_byte).into();
        let current = match op {
            Op::Index { index } => lookup_table[index],
            Op::Diff { dr, dg, db } => RGBA {
                r: previous.r.wrapping_add(dr.wrapping_sub(2)),
                g: previous.g.wrapping_add(dg.wrapping_sub(2)),
                b: previous.b.wrapping_add(db.wrapping_sub(2)),
                a: previous.a,
            },
            Op::Luma { dg } => {
                let dg = dg.wrapping_sub(32);
                let next = iter.next().ok_or(QOIError::EndOfStream)?;
                let dr = (next >> 4).wrapping_sub(8);
                let db = (next & 0b00001111).wrapping_sub(8);

                RGBA {
                    r: previous.r.wrapping_add(dg.wrapping_add(dr)),
                    g: previous.g.wrapping_add(dg),
                    b: previous.b.wrapping_add(dg.wrapping_add(db)),
                    a: previous.a,
                }
            }
            Op::Run { run } => {
                // Don't make the off-by-one correction here, since we are always returning an
                // extra pixel to be pushed by the containing block
                for _ in 0..run {
                    push_rgba(&mut output, previous);
                }
                previous
            }
            Op::RGB => RGBA {
                r: *iter.next().ok_or(QOIError::EndOfStream)?,
                g: *iter.next().ok_or(QOIError::EndOfStream)?,
                b: *iter.next().ok_or(QOIError::EndOfStream)?,
                a: previous.a,
            },
            Op::RGBA => RGBA {
                r: *iter.next().ok_or(QOIError::EndOfStream)?,
                g: *iter.next().ok_or(QOIError::EndOfStream)?,
                b: *iter.next().ok_or(QOIError::EndOfStream)?,
                a: *iter.next().ok_or(QOIError::EndOfStream)?,
            },
        };
        previous = current;
        lookup_table[hash(current)] = current;
        push_rgba(&mut output, current);
    }
    Ok(output)
}

fn encode(bitmap: &[u8]) -> &[u8] {
    todo!()
}

fn main() -> Result<(), std::io::Error> {
    let input = fs::read("input.qoi")?;
    fs::write("output.raw", decode(&input).expect("QOIError")).expect("Unable to write file");
    Ok(())
}
