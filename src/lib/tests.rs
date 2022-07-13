mod tests {
    use qoi::*;

    fn make_header(len: u32) -> Header {
        Header {
            width: len,
            height: 1,
            ..Default::default()
        }
    }

    #[test]
    fn correct_header_encoding() {
        assert_eq!(
            Into::<Vec<u8>>::into(Header {
                width: 0,
                height: 0,
                channels: Channels::RGBA,
                colorspace: ColorSpace::Linear,
            }),
            b"qoif\
            \x00\x00\x00\x00\
            \x00\x00\x00\x00\
            \x04\x01"
        );

        assert_eq!(
            Into::<Vec<u8>>::into(Header {
                width: 65535,
                height: u32::MAX - 1,
                channels: Channels::RGBA,
                colorspace: ColorSpace::Linear,
            }),
            b"qoif\
            \x00\x00\xff\xff\
            \xff\xff\xff\xfe\
            \x04\x01",
            "Incorrect endian-ness"
        );

        assert_eq!(
            Into::<Vec<u8>>::into(Header {
                width: 0,
                height: 0,
                channels: Channels::RGB,
                colorspace: ColorSpace::sRGB,
            }),
            b"qoif\
            \x00\x00\x00\x00\
            \x00\x00\x00\x00\
            \x03\x00",
            "Incorrect color information encoding"
        );
    }

    #[test]
    fn empty_header_empty_body() {
        let empty_header: Header = Default::default();
        let empty_buffer = vec![];
        let decoded = decode(empty_header, &mut empty_buffer.iter()).unwrap();
        assert_eq!(
            decoded.len(),
            0,
            "Decoding a zero-size header should produce an empty image"
        );
    }

    //#[test]
    fn empty_header_full_body() {
        let empty_header: Header = Default::default();
        let full_buffer = vec![0xfd, 0xfd, 0xfd];
        let decoded = decode(empty_header, &mut full_buffer.iter());
        assert_eq!(
            decoded,
            Err(QOIError::TooMuchInput),
            "Decoding a zero-size header should produce an empty image"
        );
    }

    #[test]
    fn test_runs() {
        let header = make_header(66);

        let body: Vec<u8> = vec![
            Op::Run { run: 3 }.into(),
            Op::Run { run: 1 }.into(),
            Op::Run { run: 62 }.into(),
        ];

        dbg!(&body);

        let buffer = decode(header, &mut body.iter()).unwrap();

        assert_eq!(buffer.len(), 66 * 4, "Run lengths are not working properly");
    }
}
