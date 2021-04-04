mod line_counter_tests {
    use panfix::line_counter::LineCounter;

    #[test]
    fn test_line_col() {
        //                              012 3 4567  890
        let counter = LineCounter::new("ab\n\ncd\r\nef");
        assert_eq!(counter.line_col(0), (0, 0));
        assert_eq!(counter.line_col(1), (0, 1));
        assert_eq!(counter.line_col(2), (0, 2));
        assert_eq!(counter.line_col(3), (1, 0));
        assert_eq!(counter.line_col(4), (2, 0));
        assert_eq!(counter.line_col(5), (2, 1));
        assert_eq!(counter.line_col(6), (2, 2));
        assert_eq!(counter.line_col(7), (2, 3));
        assert_eq!(counter.line_col(8), (3, 0));
        assert_eq!(counter.line_col(9), (3, 1));
        assert_eq!(counter.num_lines(), 4);
    }

    #[test]
    fn test_line_span() {
        //                              012 3 4567  890
        let counter = LineCounter::new("ab\n\ncd\r\nef");
        assert_eq!(counter.num_lines(), 4);
        assert_eq!(counter.line_span(0), (0, 2));
        assert_eq!(counter.line_span(1), (3, 3));
        assert_eq!(counter.line_span(2), (4, 6));
        assert_eq!(counter.line_span(3), (8, 10));
    }

    #[test]
    fn test_line_span_inclusive() {
        //                              012 3 4567  890
        let counter = LineCounter::new("ab\n\ncd\r\nef");
        assert_eq!(counter.num_lines(), 4);
        assert_eq!(counter.line_span_inclusive(0), (0, 3));
        assert_eq!(counter.line_span_inclusive(1), (3, 4));
        assert_eq!(counter.line_span_inclusive(2), (4, 8));
        assert_eq!(counter.line_span_inclusive(3), (8, 10));
    }

    #[test]
    fn test_line_contents() {
        //                              012 3 4567  890
        let counter = LineCounter::new("ab\n\ncd\r\nef");
        assert_eq!(counter.num_lines(), 4);
        assert_eq!(counter.line_contents(0), "ab");
        assert_eq!(counter.line_contents(1), "");
        assert_eq!(counter.line_contents(2), "cd");
        assert_eq!(counter.line_contents(3), "ef");
    }

    #[test]
    fn test_line_contents_inclusive() {
        //                              012 3 4567  890
        let counter = LineCounter::new("ab\n\ncd\r\nef");
        assert_eq!(counter.num_lines(), 4);
        assert_eq!(counter.line_contents_inclusive(0), "ab\n");
        assert_eq!(counter.line_contents_inclusive(1), "\n");
        assert_eq!(counter.line_contents_inclusive(2), "cd\r\n");
        assert_eq!(counter.line_contents_inclusive(3), "ef");
    }

    #[test]
    fn test_small_strings() {
        // A little ambiguous, but seems acceptable
        let counter_0 = LineCounter::new("");
        assert_eq!(counter_0.num_lines(), 0);

        let counter_1 = LineCounter::new("a");
        assert_eq!(counter_1.num_lines(), 1);
        assert_eq!(counter_1.line_col(0), (0, 0));
        assert_eq!(counter_1.line_contents(0), "a");
        assert_eq!(counter_1.line_contents_inclusive(0), "a");

        let counter_2 = LineCounter::new("\r\n");
        assert_eq!(counter_2.num_lines(), 1);
        assert_eq!(counter_2.line_col(0), (0, 0));
        assert_eq!(counter_2.line_contents(0), "");
        assert_eq!(counter_2.line_contents_inclusive(0), "\r\n");

        let counter_3 = LineCounter::new("\r\n\r\n");
        assert_eq!(counter_3.num_lines(), 2);
        assert_eq!(counter_3.line_col(0), (0, 0));
        assert_eq!(counter_3.line_col(1), (0, 1));
        assert_eq!(counter_3.line_col(2), (1, 0));
        assert_eq!(counter_3.line_col(3), (1, 1));
        assert_eq!(counter_3.line_contents(0), "");
        assert_eq!(counter_3.line_contents_inclusive(0), "\r\n");
        assert_eq!(counter_3.line_contents(1), "");
        assert_eq!(counter_3.line_contents_inclusive(1), "\r\n");
    }
}
