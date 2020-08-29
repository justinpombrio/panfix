use std::iter::Iterator;

static WHITESPACE: &str = " \t\n";
static DELIMS: &str = "()[]{}\\.,:;";

trait TokenType: Copy + std::fmt::Debug + std::fmt::Display + std::hash::Hash + Eq {
    const JUXTAPOSE: TokenType;
    const MISSING: TokenType;
}

#[derive(Debug)]
pub struct Tokenizer<'s> {
    source: &'s str,
    remaining: &'s str,
}

impl<'s> Tokenizer<'s> {
    pub fn new(source: &'s str) -> Tokenizer<'s> {
        Tokenizer {
            source,
            remaining: source,
        }
    }
}

impl<'s> Tokenizer<'s> {
    /// Consume all leading whitespace, including comments (#).
    fn skip_whitespace(&mut self) {
        let mut in_comment = false;
        for (i, ch) in self.remaining.char_indices() {
            if in_comment {
                match ch {
                    '\n' => in_comment = false,
                    _ => (),
                }
            } else {
                match ch {
                    ch if WHITESPACE.contains(ch) => (),
                    '#' => in_comment = true,
                    _ => {
                        self.remaining = &self.remaining[i..];
                        return;
                    }
                }
            }
        }
        self.remaining = "";
    }

    /// Consume a token, from the current position up to end_index.
    fn consume_token(&mut self, end_index: usize) -> &'s str {
        let token = &self.remaining[0..end_index];
        self.remaining = &self.remaining[end_index..];
        token
    }
}

impl<'s> Iterator for Tokenizer<'s> {
    type Item = &'s str;

    fn next(&mut self) -> Option<&'s str> {
        self.skip_whitespace();
        for (i, ch) in self.remaining.char_indices() {
            match ch {
                ch if DELIMS.contains(ch) => {
                    if i == 0 {
                        // First char was a delimeter. Immediately return it.
                        return Some(self.consume_token(1));
                    } else {
                        // We found a delimeter. Everything to its left is the token.
                        return Some(self.consume_token(i));
                    }
                }
                ch if ch == '#' || WHITESPACE.contains(ch) => {
                    // We found whitespace. Everything to its left is the token.
                    return Some(self.consume_token(i));
                }
                _ => (),
            }
        }
        if self.remaining.is_empty() {
            // End of the source; no tokens left.
            None
        } else {
            // We found the end of the source. Everything to its left is the token.
            Some(self.consume_token(self.remaining.len()))
        }
    }
}

#[test]
fn test_tokenize() {
    fn tokenize(source: &str) -> Vec<String> {
        Tokenizer::new(source)
            .map(|tok| tok.to_string())
            .collect::<Vec<_>>()
    }
    assert_eq!(tokenize(""), Vec::<&'static str>::new());
    assert_eq!(tokenize(" "), Vec::<&'static str>::new());
    assert_eq!(tokenize(" ! "), vec!["!"]);
    assert_eq!(
        tokenize("!@@  $ jyj %) ^& *(([5--7= _   +/, >?;wef:"),
        vec![
            "!@@", "$", "jyj", "%", ")", "^&", "*", "(", "(", "[", "5--7=", "_", "+/", ",", ">?",
            ";", "wef", ":"
        ],
    );
    assert_eq!(
        tokenize("foo##bar[]\nbaz  # \n# # waz\n  wiggles"),
        vec!["foo", "baz", "wiggles",],
    );
}
