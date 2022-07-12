use std::fs;

#[derive(Clone, Copy)]
struct Header {
    width: u32,
    height: u32,
    channels: Channels,
    colorspace: ColorSpace,
}

#[derive(Clone, Copy)]
enum Channels {
    RGB,
    RGBA,
}

#[derive(Clone, Copy)]
#[allow(non_camel_case_types)]
enum ColorSpace {
    sRGB,
    Linear,
}

#[repr(packed)]
#[derive(Copy, Clone, PartialEq)]
struct RGBA {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

#[allow(non_camel_case_types)]
type u6 = u8;
#[allow(non_camel_case_types)]
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
    MissingMagic,
    InvalidChannelSpec,
    InvalidColorSpaceSpec,
}

fn push_rgba(vec: &mut Vec<u8>, pixel: RGBA) {
    vec.push(pixel.r);
    vec.push(pixel.g);
    vec.push(pixel.b);
    vec.push(pixel.a);
}

fn write_qoi(vec: &mut Vec<u8>, op: Op) {
    vec.push(match op {
        Op::RGBA => 0xff,
        Op::Index { index } => (index & 0b00111111).try_into().unwrap(),
        Op::Run { run } => run | 0b11000000,
        _ => todo!(),
    })
}

fn parse_header<'a>(bytes: &mut impl Iterator<Item = &'a u8>) -> Result<Header, QOIError> {
    let magic = bytes.by_ref().take(4).cloned().collect::<Vec<u8>>();
    if magic.len() != 4 {
        return Err(QOIError::EndOfStream);
    };
    if magic != b"qoif" {
        return Err(QOIError::MissingMagic);
    };

    let header_bytes = bytes.by_ref().take(10).cloned().collect::<Vec<u8>>();
    if header_bytes.len() != 10 {
        return Err(QOIError::EndOfStream);
    }
    return Ok(Header {
        width: u32::from_be_bytes(header_bytes[0..4].try_into().unwrap()),
        height: u32::from_be_bytes(header_bytes[4..8].try_into().unwrap()),
        channels: match header_bytes[8] {
            3 => Channels::RGB,
            4 => Channels::RGBA,
            _ => return Err(QOIError::InvalidChannelSpec),
        },
        colorspace: match header_bytes[9] {
            0 => ColorSpace::sRGB,
            1 => ColorSpace::Linear,
            _ => return Err(QOIError::InvalidColorSpaceSpec),
        },
    });
}

// TODO: Change this to an iterator API
fn decode<'a>(params: Header, qoi: &mut impl Iterator<Item = &'a u8>) -> Result<Vec<u8>, QOIError> {
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

    let iter = qoi;

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
                for _ in 0..run + 1 {
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

fn get_rgba<'a>(iter: &mut impl Iterator<Item = &'a u8>) -> Result<RGBA, QOIError> {
    Ok(RGBA {
        r: *iter.next().ok_or(QOIError::EndOfStream)?,
        g: *iter.next().ok_or(QOIError::EndOfStream)?,
        b: *iter.next().ok_or(QOIError::EndOfStream)?,
        a: *iter.next().ok_or(QOIError::EndOfStream)?,
    })
}

fn encode<'a>(
    header: Header,
    bitmap: &mut impl Iterator<Item = &'a u8>,
) -> Result<Vec<u8>, QOIError> {
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

    let mut output = vec![b'q', b'o', b'i', b'f'];
    output.extend(header.width.to_be_bytes());
    output.extend(header.height.to_be_bytes());
    output.push(4);
    output.push(0);

    let mut iter = bitmap;

    let total = header.width * header.height;
    println!("total pixels to encode: {}", total);
    let mut num_pixels = 0;
    'outer: loop {
        if num_pixels == total {
            break;
        }
        let mut pixel = get_rgba(&mut iter).unwrap();
        num_pixels += 1;
        if pixel == previous {
            let mut run = 0;
            while {
                pixel = get_rgba(&mut iter)?;
                pixel == previous
            } {
                num_pixels += 1;
                run += 1;
                if num_pixels == total {
                    break 'outer;
                }
                if run == 61 {
                    write_qoi(&mut output, Op::Run { run });
                    run = 0;
                }
            }
            if run > 0 {
                write_qoi(&mut output, Op::Run { run: run - 1 });
            }
        }
        previous = pixel;

        let index = hash(pixel);
        if lookup_table[index] == pixel {
            write_qoi(&mut output, Op::Index { index });
            continue;
        }

        write_qoi(&mut output, Op::RGBA);
        push_rgba(&mut output, pixel);

        lookup_table[index] = pixel;
    }

    output.extend(vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]);
    Ok(output)
}

#[cfg(test)]
mod tests {

    fn decoded_length() {
        let qoi: Vec<u8> = vec!['q', 'o', 'i', 'f'];
    }
    assert_eq!();
}

fn main() -> Result<(), std::io::Error> {
    let input_buffer = fs::read("input.qoi")?;
    let mut input_iter = input_buffer.iter();
    let header = parse_header(&mut input_iter).unwrap();
    let bitmap = decode(header, &mut input_iter).unwrap();
    fs::write("output.raw", &bitmap)?;
    dbg!(bitmap.len());
    let output = encode(header, &mut bitmap.iter()).unwrap();
    fs::write("output.qoi", output).expect("Unable to write file");
    Ok(())
}
