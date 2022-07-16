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

#[derive(Clone, Copy, Debug, PartialEq)]
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
#[allow(clippy::upper_case_acronyms)]
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

pub enum PixelBuffer {
    RGBA(Vec<u8>),
    RGB(Vec<u8>),
}

impl PixelBuffer {
    pub fn new(channels: Channels) -> PixelBuffer {
        match channels {
            Channels::RGBA => PixelBuffer::RGBA(Vec::new()),
            Channels::RGB => PixelBuffer::RGB(Vec::new()),
        }
    }

    pub fn wrap(channels: Channels, vec: Vec<u8>) -> PixelBuffer {
        match channels {
            Channels::RGBA => PixelBuffer::RGBA(vec),
            Channels::RGB => PixelBuffer::RGB(vec),
        }
    }

    /*
     * Destroy the wrapper and return the internal buffer
     */
    pub fn untyped_vec(self) -> Vec<u8> {
        match self {
            PixelBuffer::RGBA(v) => v,
            PixelBuffer::RGB(v) => v,
        }
    }

    pub fn pixel_count(&self) -> usize {
        match self {
            PixelBuffer::RGBA(vec) => vec.len() / 4,
            PixelBuffer::RGB(vec) => vec.len() / 3,
        }
    }

    fn push(&mut self, value: RGBA) {
        match self {
            PixelBuffer::RGBA(v) => {
                push_rgba(v, value);
            }
            PixelBuffer::RGB(v) => {
                push_rgb(v, value);
            }
        }
    }

    fn into_iter(self) -> Box<dyn PixelIterator> {
        match self {
            PixelBuffer::RGBA(vec) => Box::new(RGBAIterator {
                iter: Box::new(vec.into_iter()),
            }),
            PixelBuffer::RGB(vec) => Box::new(RGBIterator {
                iter: Box::new(vec.into_iter()),
            }),
        }
    }
}

trait PixelIterator: Iterator<Item = Result<RGBA, QOIError>> {}

impl PixelIterator for RGBAIterator {}
impl PixelIterator for RGBIterator {}

struct RGBAIterator {
    iter: Box<dyn Iterator<Item = u8>>,
}

struct RGBIterator {
    iter: Box<dyn Iterator<Item = u8>>,
}

impl Iterator for RGBAIterator {
    type Item = Result<RGBA, QOIError>;
    fn next(&mut self) -> Option<Self::Item> {
        // If there is nothing left in the iterator, then return None
        let bytes: Vec<u8> = self.iter.by_ref().take(4).collect();
        if bytes.is_empty() {
            return None;
        } else if bytes.len() != 4 {
            return Some(Err(QOIError::UnalignedRGBA));
        };

        Some(Ok(RGBA {
            r: bytes[0],
            g: bytes[1],
            b: bytes[2],
            a: bytes[3],
        }))
    }
}

impl Iterator for RGBIterator {
    type Item = Result<RGBA, QOIError>;
    fn next(&mut self) -> Option<Self::Item> {
        // If there is nothing left in the iterator, then return None
        let bytes: Vec<u8> = self.iter.by_ref().take(3).collect();
        if bytes.is_empty() {
            return None;
        } else if bytes.len() != 3 {
            return Some(Err(QOIError::UnalignedRGBA));
        };

        Some(Ok(RGBA {
            r: bytes[0],
            g: bytes[1],
            b: bytes[2],
            a: 255,
        }))
    }
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

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(clippy::upper_case_acronyms)]
enum Op {
    Index { index: usize },
    Diff { dr: i8, dg: i8, db: i8 },
    Luma { dg: i8 },
    Run { run: u8 },
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
    UnalignedRGBA,
    TooMuchInput,
    MissingMagic,
    MisMatchedChannels,
    InvalidChannelSpec,
    InvalidColorSpaceSpec,
    CompressionError(String),
    DecompressionError(String),
}

impl std::fmt::Debug for QOIError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let tmp;
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
                QOIError::UnalignedRGBA => "Input was not aligned to 4-byte RGBA",
                QOIError::CompressionError(s) => {
                    tmp = format!("Internal Error while compressing: {}", s);
                    &tmp
                }
                QOIError::DecompressionError(s) => {
                    tmp = format!("Internal Error while decompressing: {}", s);
                    &tmp
                }
                QOIError::MisMatchedChannels =>
                    "Incorrect PixelBuffer type passed to encode for given header",
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

fn hash(pixel: RGBA) -> usize {
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
// Or a generator when generators get mainlined
pub fn decode<'a>(
    qoi: &mut impl Iterator<Item = &'a u8>,
) -> Result<(Header, PixelBuffer), QOIError> {
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
    let header = parse_header(qoi)?;

    let mut output = PixelBuffer::new(header.channels);

    let iter = qoi;

    let total = (header.width * header.height) as usize;

    while let Some(qoi_byte) = iter.next() {
        if output.pixel_count() >= total {
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
                    output.push(previous);
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
        output.push(current);
    }
    Ok((header, output))
}

pub fn encode(header: Header, bitmap: PixelBuffer) -> Result<Vec<u8>, QOIError> {
    match &bitmap {
        PixelBuffer::RGBA(_) => {
            if header.channels == Channels::RGB {
                return Err(QOIError::MisMatchedChannels);
            }
        }
        PixelBuffer::RGB(_) => {
            if header.channels == Channels::RGBA {
                return Err(QOIError::MisMatchedChannels);
            }
        }
    }

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

    let mut iter = bitmap.into_iter();

    let total = header.width * header.height;

    let mut num_pixels = 0;
    loop {
        if num_pixels == total {
            if iter.count() == 0 {
                break;
            } else {
                return Err(QOIError::TooMuchInput);
            }
        }

        if num_pixels > total {
            return Err(QOIError::CompressionError(
                "Encoded too many pixels into QOI".to_owned(),
            ));
        }
        let mut pixel: RGBA = match iter.next() {
            Some(p) => p?,
            None => {
                dbg!(num_pixels);
                return Err(QOIError::EndOfStream);
            }
        };

        let mut run = 0;
        let mut next: Option<RGBA> = Some(pixel);
        while next.is_some() && (next.unwrap()) == previous {
            let result = iter.next();
            next = match result {
                Some(r) => Some(r?),
                None => None,
            };
            run += 1;
        }

        while run > 0 {
            let amt = u32::min(62, run);
            write_qoi(&mut output, Op::Run { run: amt as u8 });
            num_pixels += amt;
            run -= amt;
        }

        match next {
            Some(px) => pixel = px,
            None => break,
        }

        let diff = pixel - previous;
        previous = pixel;

        num_pixels += 1;
        let index = hash(pixel);
        if lookup_table[index] == pixel {
            write_qoi(&mut output, Op::Index { index });
            continue;
        }

        lookup_table[index] = pixel;

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
            } else {
                // Can output RGB only to save re-encoding alpha
                write_qoi(&mut output, Op::RGB);
                push_rgb(&mut output, pixel);
            }
        } else {
            // If nothing else works, encode the whole pixel
            write_qoi(&mut output, Op::RGBA);
            push_rgba(&mut output, pixel);
        }
    }

    output.extend(vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]);
    Ok(output)
}
