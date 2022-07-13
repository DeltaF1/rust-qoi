#[derive(Clone, Copy, Debug)]
pub struct Header {
    pub width: u32,
    pub height: u32,
    pub channels: Channels,
    pub colorspace: ColorSpace,
}

impl Default for Header {
    fn default() -> Header {
        Header {
            width: 0,
            height: 0,
            channels: Channels::RGBA,
            colorspace: ColorSpace::Linear,
        }
    }
}

impl From<Header> for Vec<u8> {
    fn from(header: Header) -> Vec<u8> {
        let mut v = vec![b'q', b'o', b'i', b'f'];
        v.extend(header.width.to_be_bytes());
        v.extend(header.height.to_be_bytes());
        v.push(header.channels as u8);
        v.push(header.colorspace as u8);
        v
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum Channels {
    RGB = 3,
    RGBA = 4,
}

#[derive(Clone, Copy, Debug)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum ColorSpace {
    sRGB = 0,
    Linear = 1,
}

#[repr(packed)]
#[derive(Copy, Clone, PartialEq, Debug)]
struct RGBA {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

struct Diff {
    dr: i8,
    dg: i8,
    db: i8,
    da: i8,
}

impl std::ops::Sub for RGBA {
    type Output = Diff;

    fn sub(self, rhs: RGBA) -> Diff {
        Diff {
            dr: self.r.wrapping_sub(rhs.r) as i8,
            dg: self.g.wrapping_sub(rhs.g) as i8,
            db: self.b.wrapping_sub(rhs.b) as i8,
            da: self.a.wrapping_sub(rhs.a) as i8,
        }
    }
}

#[allow(non_camel_case_types)]
type i6 = i8;
#[allow(non_camel_case_types)]
type u6 = u8;
#[allow(non_camel_case_types)]
type i2 = i8;
type Index = usize;

#[derive(Debug, PartialEq, Clone, Copy)]
enum Op {
    Index { index: Index },
    Diff { dr: i2, dg: i2, db: i2 },
    Luma { dg: i6 },
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
                dr: (get_2bits(byte, 4) as i8).wrapping_sub(2),
                dg: (get_2bits(byte, 2) as i8).wrapping_sub(2),
                db: (get_2bits(byte, 0) as i8).wrapping_sub(2),
            },
            0b10 => Op::Luma {
                dg: ((byte & 0b00111111) as i8).wrapping_sub(32),
            },
            0b11 => match byte {
                0xff => Op::RGBA,
                0xfe => Op::RGB,
                _ => Op::Run {
                    run: (byte & 0b00111111).wrapping_add(1),
                },
            },
            // Unreachable since u8 >> 6 is only 2 bits
            4_u8..=u8::MAX => unreachable!(),
        }
    }
}

impl From<Op> for u8 {
    fn from(op: Op) -> u8 {
        match op {
            Op::Index { index } => (index & 0b00111111).try_into().unwrap(),
            Op::Run { run } => run.wrapping_sub(1) | 0b11000000,
            Op::Diff { dr, dg, db } => {
                0b01000000
                    | (dr.wrapping_add(2) << 4) as u8
                    | (dg.wrapping_add(2) << 2) as u8
                    | db.wrapping_add(2) as u8
            }
            Op::Luma { dg } => 0b10000000 | dg.wrapping_add(32) as u8,
            Op::RGB => 0b11111110,
            Op::RGBA => 0b11111111,
        }
    }
}

#[derive(PartialEq)]
pub enum QOIError {
    EndOfStream,
    TooMuchInput,
    MissingMagic,
    InvalidChannelSpec,
    InvalidColorSpaceSpec,
}

impl std::fmt::Debug for QOIError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                QOIError::EndOfStream => "Expected more input",
                QOIError::TooMuchInput => "Did not expect any more input",
                QOIError::MissingMagic =>
                    "Expected header to start with the 4-byte ASCII literal 'qoif'",
                QOIError::InvalidChannelSpec =>
                    "Invalid number of channels, expected RGB (3) or RGBA (4)",
                QOIError::InvalidColorSpaceSpec =>
                    "Invalid color space, expected sRGB (0) or linear (1)", // TODO: Capture the invalid byte in enum
            }
        )
    }
}

#[cfg(test)]
mod tests;

#[cfg(test)]
mod representation_tests;

fn get_2bits(byte: u8, offset: u8) -> u8 {
    (byte >> offset) & 0b11
}

fn hash(pixel: RGBA) -> Index {
    ((pixel.r.wrapping_mul(3)).wrapping_add(
        (pixel.g.wrapping_mul(5))
            .wrapping_add((pixel.b.wrapping_mul(7)).wrapping_add(pixel.a.wrapping_mul(11))),
    ) % 64)
        .into()
}
fn push_rgba(vec: &mut Vec<u8>, pixel: RGBA) {
    vec.push(pixel.r);
    vec.push(pixel.g);
    vec.push(pixel.b);
    vec.push(pixel.a);
}

fn push_rgb(vec: &mut Vec<u8>, pixel: RGBA) {
    vec.push(pixel.r);
    vec.push(pixel.g);
    vec.push(pixel.b);
}
fn write_qoi(vec: &mut Vec<u8>, op: Op) {
    vec.push(op.into());
}

pub fn parse_header<'a>(bytes: &mut impl Iterator<Item = &'a u8>) -> Result<Header, QOIError> {
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

    Ok(Header {
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
    })
}

// TODO: Change this to an iterator API
pub fn decode<'a>(
    params: Header,
    qoi: &mut impl Iterator<Item = &'a u8>,
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
    let mut output = vec![];

    let iter = qoi;

    let total: u32 = params.width * params.height * (params.channels as u32);

    while let Some(qoi_byte) = iter.next() {
        if output.len() >= total.try_into().unwrap() {
            break;
        }
        let op: Op = (*qoi_byte).into();
        let current = match op {
            Op::Index { index } => lookup_table[index],
            Op::Diff { dr, dg, db } => RGBA {
                r: previous.r.wrapping_add(dr as u8),
                g: previous.g.wrapping_add(dg as u8),
                b: previous.b.wrapping_add(db as u8),
                a: previous.a,
            },
            Op::Luma { dg } => {
                let next = iter.next().ok_or(QOIError::EndOfStream)?;
                let dr = (next >> 4).wrapping_sub(8) as i8;
                let db = (next & 0b00001111).wrapping_sub(8) as i8;

                RGBA {
                    r: previous.r.wrapping_add(dg.wrapping_add(dr) as u8),
                    g: previous.g.wrapping_add(dg as u8),
                    b: previous.b.wrapping_add(dg.wrapping_add(db) as u8),
                    a: previous.a,
                }
            }
            Op::Run { run } => {
                // Don't make the off-by-one correction here, since we are always returning an
                // extra pixel to be pushed by the containing block
                for _ in 0..(run - 1) {
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

pub fn encode<'a>(
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

    let mut output: Vec<u8> = header.into();
    let mut iter = bitmap;

    let total = header.width * header.height;
    println!("total pixels to encode: {}", total);
    let mut num_pixels = 0;
    'outer: loop {
        if num_pixels == total {
            println!("Early break!");
            println!("{} left in iter", iter.count());
            break;
        }
        let mut pixel = match get_rgba(&mut iter) {
            Ok(p) => p,
            Err(e) => {
                dbg!(num_pixels);
                return Err(e);
            }
        };

        let mut run = 0;
        while pixel == previous {
            pixel = get_rgba(&mut iter)?;
            run += 1;
        }

        while run > 0 {
            let amt = u32::min(62, run);
            write_qoi(&mut output, Op::Run { run: amt as u8 });
            num_pixels += amt;
            run = run - amt;
        }

        num_pixels += 1;
        let index = hash(pixel);
        if lookup_table[index] == pixel {
            write_qoi(&mut output, Op::Index { index });
            continue;
        }

        lookup_table[index] = pixel;

        let diff = pixel - previous;

        previous = pixel;

        if diff.da == 0 {
            let dr_dg = diff.dr.wrapping_sub(diff.dg);
            let db_dg = diff.db.wrapping_sub(diff.dg);
            if (-2..=1).contains(&(diff.dr))
                && (-2..=1).contains(&(diff.dg))
                && (-2..=1).contains(&(diff.db))
            {
                write_qoi(
                    &mut output,
                    Op::Diff {
                        dr: diff.dr,
                        dg: diff.dg,
                        db: diff.db,
                    },
                );
            } else if (-32..=31).contains(&diff.dg.into())
                && (-8..=7).contains(&dr_dg.into())
                && (-8..=7).contains(&db_dg.into())
            {
                write_qoi(&mut output, Op::Luma { dg: diff.dg });

                let second_byte: u8 =
                    (dr_dg as u8).wrapping_add(8) << 4 | ((db_dg as u8).wrapping_add(8) & 0x0f);

                output.push(second_byte);
                continue;
            } else {
                // Can output RGB to save re-encoding alpha
                write_qoi(&mut output, Op::RGB);
                push_rgb(&mut output, pixel);
            }
        }

        // If nothing else works, encode the whole pixel
        write_qoi(&mut output, Op::RGBA);
        push_rgba(&mut output, pixel);
    }

    output.extend(vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]);
    Ok(output)
}
