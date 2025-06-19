use panfix::{Position, Source};

#[test]
fn test_source() {
    let source = Source::new("TEST", "line\n".to_owned());
    assert_eq!(
        source.end_of_file(),
        Position {
            line: 0,
            col: 4,
            utf8_col: 4
        }
    );
}
