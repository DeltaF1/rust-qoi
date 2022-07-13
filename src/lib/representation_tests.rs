mod representation_tests {
    use super::Op;

    fn transmogrify(op: Op) -> (Op, Op) {
        let intermediate: u8 = op.into();
        return (op, intermediate.into());
    }

    #[test]
    fn idx() {
        let idx_op = transmogrify(Op::Index { index: 63 });
        assert_eq!(idx_op.0, idx_op.1);

        let idx_op = transmogrify(Op::Index { index: 0 });
        assert_eq!(idx_op.0, idx_op.1);

        let idx_op = transmogrify(Op::Index { index: 62 });
        assert_eq!(idx_op.0, idx_op.1);

        let idx_op = transmogrify(Op::Index { index: 1 });
        assert_eq!(idx_op.0, idx_op.1);
    }

    #[test]
    fn diff() {
        let diff_op = transmogrify(Op::Diff {
            dr: 1,
            dg: -2,
            db: 0,
        });
        assert_eq!(diff_op.0, diff_op.1);

        let diff_op = transmogrify(Op::Diff {
            dr: 0,
            dg: 0,
            db: 0,
        });
        assert_eq!(diff_op.0, diff_op.1);

        let diff_op = transmogrify(Op::Diff {
            dr: -1,
            dg: -1,
            db: -1,
        });
        assert_eq!(diff_op.0, diff_op.1);
    }

    #[test]
    #[should_panic]
    fn diff_out_of_range() {
        let diff_op = transmogrify(Op::Diff {
            dr: 1,
            dg: -20,
            db: 0,
        });
        assert_eq!(diff_op.0, diff_op.1);
    }

    #[test]
    fn run() {
        let run_op = transmogrify(Op::Run { run: 1 });
        assert_eq!(run_op.0, run_op.1);

        let run_op = transmogrify(Op::Run { run: 2 });
        assert_eq!(run_op.0, run_op.1);

        let run_op = transmogrify(Op::Run { run: 61 });
        assert_eq!(run_op.0, run_op.1);

        let run_op = transmogrify(Op::Run { run: 62 });
        assert_eq!(run_op.0, run_op.1);
    }

    #[test]
    fn run_invalid_length() {
        let run_op = transmogrify(Op::Run { run: 63 });
        assert_ne!(run_op.0, run_op.1);

        let run_op = transmogrify(Op::Run { run: 64 });
        assert_ne!(run_op.0, run_op.1);

        let run_op = transmogrify(Op::Run { run: 100 });
        assert_ne!(run_op.0, run_op.1);

        let run_op = transmogrify(Op::Run { run: 0 });
        assert_ne!(run_op.0, run_op.1);
    }
}
