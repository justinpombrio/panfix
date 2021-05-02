mod line_and_col_indexer_tests {
    use panfix::line_and_col_indexer::LineAndColIndexer;

    #[test]
    fn test_line_col() {
        //                                    012 3 4567  890
        let indexer = LineAndColIndexer::new("ab\n\ncd\r\nef");
        assert_eq!(indexer.line_col(0), (0, 0));
        assert_eq!(indexer.line_col(1), (0, 1));
        assert_eq!(indexer.line_col(2), (0, 2));
        assert_eq!(indexer.line_col(3), (1, 0));
        assert_eq!(indexer.line_col(4), (2, 0));
        assert_eq!(indexer.line_col(5), (2, 1));
        assert_eq!(indexer.line_col(6), (2, 2));
        assert_eq!(indexer.line_col(7), (2, 3));
        assert_eq!(indexer.line_col(8), (3, 0));
        assert_eq!(indexer.line_col(9), (3, 1));
        assert_eq!(indexer.num_lines(), 4);
    }

    #[test]
    fn test_line_span() {
        //                              012 3 4567  890
        let indexer = LineAndColIndexer::new("ab\n\ncd\r\nef");
        assert_eq!(indexer.num_lines(), 4);
        assert_eq!(indexer.line_span(0), (0, 2));
        assert_eq!(indexer.line_span(1), (3, 3));
        assert_eq!(indexer.line_span(2), (4, 6));
        assert_eq!(indexer.line_span(3), (8, 10));
    }

    #[test]
    fn test_line_span_inclusive() {
        //                              012 3 4567  890
        let indexer = LineAndColIndexer::new("ab\n\ncd\r\nef");
        assert_eq!(indexer.num_lines(), 4);
        assert_eq!(indexer.line_span_inclusive(0), (0, 3));
        assert_eq!(indexer.line_span_inclusive(1), (3, 4));
        assert_eq!(indexer.line_span_inclusive(2), (4, 8));
        assert_eq!(indexer.line_span_inclusive(3), (8, 10));
    }

    #[test]
    fn test_line_contents() {
        //                              012 3 4567  890
        let indexer = LineAndColIndexer::new("ab\n\ncd\r\nef");
        assert_eq!(indexer.num_lines(), 4);
        assert_eq!(indexer.line_contents(0), "ab");
        assert_eq!(indexer.line_contents(1), "");
        assert_eq!(indexer.line_contents(2), "cd");
        assert_eq!(indexer.line_contents(3), "ef");
    }

    #[test]
    fn test_line_contents_inclusive() {
        //                              012 3 4567  890
        let indexer = LineAndColIndexer::new("ab\n\ncd\r\nef");
        assert_eq!(indexer.num_lines(), 4);
        assert_eq!(indexer.line_contents_inclusive(0), "ab\n");
        assert_eq!(indexer.line_contents_inclusive(1), "\n");
        assert_eq!(indexer.line_contents_inclusive(2), "cd\r\n");
        assert_eq!(indexer.line_contents_inclusive(3), "ef");
    }

    #[test]
    fn test_small_strings() {
        // A little ambiguous, but seems acceptable
        let indexer_0 = LineAndColIndexer::new("");
        assert_eq!(indexer_0.num_lines(), 0);

        let indexer_1 = LineAndColIndexer::new("a");
        assert_eq!(indexer_1.num_lines(), 1);
        assert_eq!(indexer_1.line_col(0), (0, 0));
        assert_eq!(indexer_1.line_contents(0), "a");
        assert_eq!(indexer_1.line_contents_inclusive(0), "a");

        let indexer_2 = LineAndColIndexer::new("\r\n");
        assert_eq!(indexer_2.num_lines(), 1);
        assert_eq!(indexer_2.line_col(0), (0, 0));
        assert_eq!(indexer_2.line_contents(0), "");
        assert_eq!(indexer_2.line_contents_inclusive(0), "\r\n");

        let indexer_3 = LineAndColIndexer::new("\r\n\r\n");
        assert_eq!(indexer_3.num_lines(), 2);
        assert_eq!(indexer_3.line_col(0), (0, 0));
        assert_eq!(indexer_3.line_col(1), (0, 1));
        assert_eq!(indexer_3.line_col(2), (1, 0));
        assert_eq!(indexer_3.line_col(3), (1, 1));
        assert_eq!(indexer_3.line_contents(0), "");
        assert_eq!(indexer_3.line_contents_inclusive(0), "\r\n");
        assert_eq!(indexer_3.line_contents(1), "");
        assert_eq!(indexer_3.line_contents_inclusive(1), "\r\n");
    }
}
