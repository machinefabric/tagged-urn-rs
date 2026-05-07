//! Flat Tag-Based URN Identifier System
//!
//! This module provides a flat, tag-based tagged URN system with configurable
//! prefixes, wildcard support, and specificity comparison.

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::BTreeMap;
use std::fmt;
use std::str::FromStr;

/// A tagged URN using flat, ordered tags with a configurable prefix
///
/// Examples:
/// - `cap:generate;ext=pdf;output=binary;target=thumbnail`
/// - `myapp:key="Value With Spaces"`
/// - `custom:a=1;b=2`
#[derive(Debug, Clone, Eq, Hash)]
pub struct TaggedUrn {
    /// The prefix for this URN (e.g., "cap", "myapp", "custom")
    pub prefix: String,
    /// The tags that define this URN, stored in sorted order for canonical representation
    pub tags: BTreeMap<String, String>,
}

impl PartialEq for TaggedUrn {
    fn eq(&self, other: &Self) -> bool {
        self.prefix == other.prefix && self.tags == other.tags
    }
}

impl PartialOrd for TaggedUrn {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TaggedUrn {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Compare by prefix first, then by tags (BTreeMap comparison is lexicographic)
        match self.prefix.cmp(&other.prefix) {
            std::cmp::Ordering::Equal => self.tags.cmp(&other.tags),
            other => other,
        }
    }
}

/// Parser states for the state machine.
///
/// The parser handles six tag forms — the canonical alphabet of the
/// constraint truth table:
///
/// | Authored                | Canonical | Stored value | Score | Reading                                  |
/// |-------------------------|-----------|--------------|------:|------------------------------------------|
/// | `?x` ≡ `x?`             | `?x`      | `"?"`        |     0 | no constraint                            |
/// | `?x=v` ≡ `x?=v`         | `x?=v`    | `"?=v"`      |     1 | absent OR (present and not v)            |
/// | `x` ≡ `x=*`             | `x`       | `"*"`        |     2 | present with any value                   |
/// | `!x=v` ≡ `x!=v`         | `x!=v`    | `"!=v"`      |     3 | present and not v                        |
/// | `x=v`                   | `x=v`     | `"v"`        |     4 | present and exactly v (`v ∉ {?, !, *}`)  |
/// | `!x` ≡ `x!`             | `!x`      | `"!"`        |     5 | absent (must-not-have)                   |
///
/// The qualifier `?` or `!` may appear EITHER as a key prefix
/// (`?x`, `!x`, `?x=v`, `!x=v`) OR as an infix immediately before `=`
/// (`x?`, `x!`, `x?=v`, `x!=v`). The two notations are exact aliases;
/// the parser collapses both to the same canonical storage.
///
/// **Disallowed** — these are hard parse errors, not silently
/// accepted shorthands:
/// - `x=?v`, `x=!v`: a value starting with `?` or `!` is not a
///   qualifier; it would be an exact value, but exact values may not
///   start with `?` or `!` (reserved for syntactic qualifiers). Use
///   `x?=v` or `x!=v` for the qualified forms.
/// - `?x?`, `?x?=v`, `!x!=v`, `!x?`, `?!x`, `!?x`: mixing prefix and
///   infix qualifiers, or mixing `?` and `!`, is contradictory.
/// - `?x=*`, `!x=*`, `?x=` (empty value): a `?`/`!` qualifier with
///   `*` or empty contradicts the qualifier's own semantics.
/// - `x=`: empty exact value.
#[derive(Debug, Clone, Copy, PartialEq)]
enum ParseState {
    ExpectingKey,
    /// Saw a leading `?` at key position; the next character must
    /// begin a key. After the key, the only valid follow-ups are
    /// `;`/end (canonical `?x`) or `=v` (canonical `x?=v`).
    AfterPrefixQuestion,
    /// Saw a leading `!` at key position; same shape as above with
    /// `!` semantics. After the key: `;`/end (canonical `!x`) or
    /// `=v` (canonical `x!=v`).
    AfterPrefixBang,
    InKey,
    /// In a key, saw `?`. Awaiting `=` to confirm infix qualifier
    /// (`x?=v`) or `;`/end to confirm bare-suffix (`x?` ≡ `?x`).
    /// Anything else is a parse error.
    InKeyAfterQuestion,
    /// Same as above for `!` — `x!` (canonical `!x`) or `x!=v`.
    InKeyAfterBang,
    ExpectingValue,
    InUnquotedValue,
    InQuotedValue,
    InQuotedValueEscape,
    ExpectingSemiOrEnd,
}

/// Per-tag truth-table specificity score. Applied uniformly to any
/// stored tag value; missing keys score 0 (the caller filters them
/// out before calling this).
///
/// | Stored value     | Form           | Score |
/// |------------------|----------------|------:|
/// | `"?"`            | `?x`           |     0 |
/// | starts with `?=` | `x?=v`         |     1 |
/// | `"*"`            | `x` (`x=*`)    |     2 |
/// | starts with `!=` | `x!=v`         |     3 |
/// | exact value      | `x=v`          |     4 |
/// | `"!"`            | `!x`           |     5 |
pub fn score_tag_value(value: &str) -> usize {
    match value {
        "?" => 0,
        "*" => 2,
        "!" => 5,
        v if v.starts_with("?=") => 1,
        v if v.starts_with("!=") => 3,
        _ => 4,
    }
}

/// Internal classification of a tag's stored value into one of the
/// six canonical constraint forms (plus the implicit "missing" form
/// for keys with no entry). Used by the truth-table matcher and
/// specificity scorer; never serialized.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Form<'a> {
    /// Key absent from the tag map.
    Missing,
    /// Stored as `"?"` — no constraint.
    NoConstraint,
    /// Stored as `"?=v"` — absent OR (present and not v).
    AbsentOrNotValue(&'a str),
    /// Stored as `"*"` — present with any value.
    MustHaveAny,
    /// Stored as `"!=v"` — present and not v.
    PresentNotValue(&'a str),
    /// Stored as a non-sigil string — present and exactly equal.
    Exact(&'a str),
    /// Stored as `"!"` — absent (must-not-have).
    MustNotHave,
}

impl TaggedUrn {
    /// Create a new tagged URN from tags with a specified prefix
    /// Keys are normalized to lowercase; values are preserved as-is
    pub fn new(prefix: String, tags: BTreeMap<String, String>) -> Self {
        let normalized_tags = tags
            .into_iter()
            .map(|(k, v)| (k.to_lowercase(), v))
            .collect();
        Self {
            prefix: prefix.to_lowercase(),
            tags: normalized_tags,
        }
    }

    /// Create an empty tagged URN with the specified prefix
    pub fn empty(prefix: String) -> Self {
        Self {
            prefix: prefix.to_lowercase(),
            tags: BTreeMap::new(),
        }
    }

    /// Create a tagged URN from a string representation
    ///
    /// Format: `prefix:key1=value1;key2=value2;...` or `prefix:key1="value with spaces";key2=simple`
    /// The prefix is required and ends at the first colon
    /// Trailing semicolons are optional and ignored
    /// Tags are automatically sorted alphabetically for canonical form
    ///
    /// Case handling:
    /// - Prefix: Normalized to lowercase
    /// - Keys: Always normalized to lowercase
    /// - Unquoted values: Normalized to lowercase
    /// - Quoted values: Case preserved exactly as specified
    pub fn from_string(s: &str) -> Result<Self, TaggedUrnError> {
        // Fail hard on leading/trailing whitespace
        if s != s.trim() {
            return Err(TaggedUrnError::WhitespaceInInput(s.to_string()));
        }

        if s.is_empty() {
            return Err(TaggedUrnError::Empty);
        }

        // Find the prefix (everything before the first colon)
        let colon_pos = s.find(':').ok_or(TaggedUrnError::MissingPrefix)?;

        if colon_pos == 0 {
            return Err(TaggedUrnError::EmptyPrefix);
        }

        let prefix = s[..colon_pos].to_lowercase();
        let tags_part = &s[colon_pos + 1..];
        let mut tags = BTreeMap::new();

        // Handle empty tagged URN (prefix: with no tags)
        if tags_part.is_empty() || tags_part == ";" {
            return Ok(Self { prefix, tags });
        }

        let mut state = ParseState::ExpectingKey;
        let mut current_key = String::new();
        let mut current_value = String::new();
        // Tracks the qualifier for the tag currently being parsed:
        //   None      — no qualifier seen yet (the four "neutral" forms)
        //   Some('?') — `?` qualifier (prefix `?x` or infix `x?=`)
        //   Some('!') — `!` qualifier (prefix `!x` or infix `x!=`)
        // Reset to None on each finish_tag.
        let mut qualifier: Option<char> = None;
        let chars: Vec<char> = tags_part.chars().collect();
        let mut pos = 0;

        while pos < chars.len() {
            let c = chars[pos];

            match state {
                ParseState::ExpectingKey => {
                    if c == ';' {
                        // Empty segment, skip
                        pos += 1;
                        continue;
                    } else if c == '?' {
                        qualifier = Some('?');
                        state = ParseState::AfterPrefixQuestion;
                    } else if c == '!' {
                        qualifier = Some('!');
                        state = ParseState::AfterPrefixBang;
                    } else if Self::is_valid_key_char(c) {
                        current_key.push(c.to_ascii_lowercase());
                        state = ParseState::InKey;
                    } else {
                        return Err(TaggedUrnError::InvalidCharacter(format!(
                            "invalid character '{}' at position {}",
                            c, pos
                        )));
                    }
                }

                ParseState::AfterPrefixQuestion | ParseState::AfterPrefixBang => {
                    // After `?` or `!` prefix, the next character MUST
                    // begin a key. No second qualifier, no `=`, no
                    // `;` (a bare prefix-and-nothing is meaningless).
                    if Self::is_valid_key_char(c) {
                        current_key.push(c.to_ascii_lowercase());
                        state = ParseState::InKey;
                    } else {
                        let q = qualifier.unwrap();
                        return Err(TaggedUrnError::InvalidCharacter(format!(
                            "expected key character after '{}' qualifier, got '{}' at position {}",
                            q, c, pos
                        )));
                    }
                }

                ParseState::InKey => {
                    if c == '=' {
                        if current_key.is_empty() {
                            return Err(TaggedUrnError::EmptyTagComponent(
                                "empty key".to_string(),
                            ));
                        }
                        state = ParseState::ExpectingValue;
                    } else if c == '?' {
                        // Infix qualifier: `x?` so far. Next must be
                        // `=` (continue to value) or `;`/end (bare
                        // suffix, equivalent to `?x`).
                        if qualifier.is_some() {
                            return Err(TaggedUrnError::InvalidCharacter(format!(
                                "duplicate qualifier '?' at position {}: prefix and infix \
                                 qualifiers cannot be combined on the same key '{}'",
                                pos, current_key
                            )));
                        }
                        qualifier = Some('?');
                        state = ParseState::InKeyAfterQuestion;
                    } else if c == '!' {
                        if qualifier.is_some() {
                            return Err(TaggedUrnError::InvalidCharacter(format!(
                                "duplicate qualifier '!' at position {}: prefix and infix \
                                 qualifiers cannot be combined on the same key '{}'",
                                pos, current_key
                            )));
                        }
                        qualifier = Some('!');
                        state = ParseState::InKeyAfterBang;
                    } else if c == ';' {
                        // Value-less tag.
                        if current_key.is_empty() {
                            return Err(TaggedUrnError::EmptyTagComponent(
                                "empty key".to_string(),
                            ));
                        }
                        current_value = Self::canonical_no_value(qualifier);
                        Self::finish_tag(&mut tags, &mut current_key, &mut current_value)?;
                        qualifier = None;
                        state = ParseState::ExpectingKey;
                    } else if Self::is_valid_key_char(c) {
                        current_key.push(c.to_ascii_lowercase());
                    } else {
                        return Err(TaggedUrnError::InvalidCharacter(format!(
                            "invalid character '{}' in key at position {}",
                            c, pos
                        )));
                    }
                }

                ParseState::InKeyAfterQuestion | ParseState::InKeyAfterBang => {
                    // Saw `?` or `!` after a key in `InKey`. Only
                    // `=` (to continue to a value) or `;`/end (bare
                    // suffix qualifier) are valid.
                    if c == '=' {
                        state = ParseState::ExpectingValue;
                    } else if c == ';' {
                        // `x?` or `x!` alone — bare suffix, identical
                        // to `?x` or `!x`.
                        current_value = Self::canonical_no_value(qualifier);
                        Self::finish_tag(&mut tags, &mut current_key, &mut current_value)?;
                        qualifier = None;
                        state = ParseState::ExpectingKey;
                    } else {
                        let q = qualifier.unwrap();
                        return Err(TaggedUrnError::InvalidCharacter(format!(
                            "expected '=' or ';' after '{}{}' suffix qualifier, got '{}' at position {}",
                            current_key, q, c, pos
                        )));
                    }
                }

                ParseState::ExpectingValue => {
                    if c == '"' {
                        state = ParseState::InQuotedValue;
                    } else if c == ';' {
                        return Err(TaggedUrnError::EmptyTagComponent(format!(
                            "empty value for key '{}'",
                            current_key
                        )));
                    } else if Self::is_valid_unquoted_value_char(c) {
                        current_value.push(c.to_ascii_lowercase());
                        state = ParseState::InUnquotedValue;
                    } else {
                        return Err(TaggedUrnError::InvalidCharacter(format!(
                            "invalid character '{}' in value at position {}",
                            c, pos
                        )));
                    }
                }

                ParseState::InUnquotedValue => {
                    if c == ';' {
                        Self::canonicalize_value(qualifier, &current_key, &mut current_value)?;
                        Self::finish_tag(&mut tags, &mut current_key, &mut current_value)?;
                        qualifier = None;
                        state = ParseState::ExpectingKey;
                    } else if Self::is_valid_unquoted_value_char(c) {
                        current_value.push(c.to_ascii_lowercase());
                    } else {
                        return Err(TaggedUrnError::InvalidCharacter(format!(
                            "invalid character '{}' in unquoted value at position {}",
                            c, pos
                        )));
                    }
                }

                ParseState::InQuotedValue => {
                    if c == '"' {
                        state = ParseState::ExpectingSemiOrEnd;
                    } else if c == '\\' {
                        state = ParseState::InQuotedValueEscape;
                    } else {
                        // Any character allowed in quoted value, preserve case
                        current_value.push(c);
                    }
                }

                ParseState::InQuotedValueEscape => {
                    if c == '"' || c == '\\' {
                        current_value.push(c);
                        state = ParseState::InQuotedValue;
                    } else {
                        return Err(TaggedUrnError::InvalidEscapeSequence(pos));
                    }
                }

                ParseState::ExpectingSemiOrEnd => {
                    if c == ';' {
                        Self::canonicalize_value(qualifier, &current_key, &mut current_value)?;
                        Self::finish_tag(&mut tags, &mut current_key, &mut current_value)?;
                        qualifier = None;
                        state = ParseState::ExpectingKey;
                    } else {
                        return Err(TaggedUrnError::InvalidCharacter(format!(
                            "expected ';' or end after quoted value, got '{}' at position {}",
                            c, pos
                        )));
                    }
                }
            }

            pos += 1;
        }

        // Handle end of input
        match state {
            ParseState::InUnquotedValue | ParseState::ExpectingSemiOrEnd => {
                Self::canonicalize_value(qualifier, &current_key, &mut current_value)?;
                Self::finish_tag(&mut tags, &mut current_key, &mut current_value)?;
            }
            ParseState::ExpectingKey => {
                // Valid — trailing semicolon or empty input after prefix.
            }
            ParseState::InQuotedValue | ParseState::InQuotedValueEscape => {
                return Err(TaggedUrnError::UnterminatedQuote(pos));
            }
            ParseState::AfterPrefixQuestion | ParseState::AfterPrefixBang => {
                let q = qualifier.unwrap();
                return Err(TaggedUrnError::EmptyTagComponent(format!(
                    "qualifier '{}' at end of input has no key",
                    q
                )));
            }
            ParseState::InKey => {
                // Value-less tag at end. Canonical form depends on
                // qualifier:
                //   None       -> "*" (bare key, must-have-any)
                //   Some('?')  -> "?" (no constraint)
                //   Some('!')  -> "!" (must-not-have)
                if current_key.is_empty() {
                    return Err(TaggedUrnError::EmptyTagComponent(
                        "empty key".to_string(),
                    ));
                }
                current_value = Self::canonical_no_value(qualifier);
                Self::finish_tag(&mut tags, &mut current_key, &mut current_value)?;
            }
            ParseState::InKeyAfterQuestion | ParseState::InKeyAfterBang => {
                // `x?` or `x!` at end of input — bare suffix
                // qualifier, no value.
                current_value = Self::canonical_no_value(qualifier);
                Self::finish_tag(&mut tags, &mut current_key, &mut current_value)?;
            }
            ParseState::ExpectingValue => {
                return Err(TaggedUrnError::EmptyTagComponent(format!(
                    "empty value for key '{}'",
                    current_key
                )));
            }
        }

        Ok(Self { prefix, tags })
    }

    /// Finish a tag by validating and inserting it
    fn finish_tag(
        tags: &mut BTreeMap<String, String>,
        key: &mut String,
        value: &mut String,
    ) -> Result<(), TaggedUrnError> {
        if key.is_empty() {
            return Err(TaggedUrnError::EmptyTagComponent("empty key".to_string()));
        }
        if value.is_empty() {
            return Err(TaggedUrnError::EmptyTagComponent(format!(
                "empty value for key '{}'",
                key
            )));
        }

        // Check for duplicate keys
        if tags.contains_key(key.as_str()) {
            return Err(TaggedUrnError::DuplicateKey(key.clone()));
        }

        // Validate key cannot be purely numeric
        if Self::is_purely_numeric(key) {
            return Err(TaggedUrnError::NumericKey(key.clone()));
        }

        tags.insert(std::mem::take(key), std::mem::take(value));
        Ok(())
    }

    /// Canonical stored value for a value-less tag, given its
    /// qualifier (if any). Used by the parser when a tag is
    /// terminated with `;`/end while in `InKey` /
    /// `InKeyAfterQuestion` / `InKeyAfterBang`.
    ///
    ///   None      -> "*"  (bare `x`, the must-have-any sigil)
    ///   Some('?') -> "?"  (`?x`, `x?`, or `x=?`, the no-constraint sigil)
    ///   Some('!') -> "!"  (`!x`, `x!`, or `x=!`, the must-not-have sigil)
    fn canonical_no_value(qualifier: Option<char>) -> String {
        match qualifier {
            None => "*".to_string(),
            Some('?') => "?".to_string(),
            Some('!') => "!".to_string(),
            Some(_) => unreachable!("qualifier may only be None, Some('?'), or Some('!')"),
        }
    }

    /// Canonicalize a parsed `(qualifier, value)` pair into the
    /// stored form on the way to `finish_tag`. The four shapes:
    ///
    ///   (None,      "*")  -> "*"     (`x=*` ≡ bare `x`)
    ///   (None,      v  )  -> v       (`x=v`, exact)
    ///   (Some('?'), v  )  -> "?=v"   (`?x=v`, `x?=v`, must be ≠ "*")
    ///   (Some('!'), v  )  -> "!=v"   (`!x=v`, `x!=v`, must be ≠ "*")
    ///
    /// Combining `?`/`!` with `*` is a contradiction (`?x=*`,
    /// `!x=*`): the qualifier and the wildcard make incompatible
    /// claims. Hard reject. Same for combining a qualifier with a
    /// sigil-only value `?` or `!` (`?x=?`, `?x=!`, etc.).
    fn canonicalize_value(
        qualifier: Option<char>,
        key: &str,
        value: &mut String,
    ) -> Result<(), TaggedUrnError> {
        match qualifier {
            None => {
                // No qualifier — value is either "*" (already
                // canonical for bare-x equivalent) or an exact
                // value. The parser already ensured non-empty.
                Ok(())
            }
            Some(q @ '?') | Some(q @ '!') => {
                // Reject `*` and the sigil-only values `?` / `!`.
                // These would conflate the qualifier semantics with
                // the bare-form semantics.
                if value == "*" || value == "?" || value == "!" {
                    return Err(TaggedUrnError::InvalidCharacter(format!(
                        "qualifier '{}' on key '{}' cannot combine with sigil value '{}': \
                         use a real value (e.g. '{}{}=v') or drop the qualifier",
                        q, key, value, q, key
                    )));
                }
                let mut canonical = String::with_capacity(value.len() + 2);
                canonical.push(q);
                canonical.push('=');
                canonical.push_str(value);
                *value = canonical;
                Ok(())
            }
            Some(other) => unreachable!(
                "qualifier may only be None, Some('?'), or Some('!'); got Some({})",
                other
            ),
        }
    }

    /// Check if character is valid for a key
    fn is_valid_key_char(c: char) -> bool {
        c.is_alphanumeric() || c == '_' || c == '-' || c == '/' || c == ':' || c == '.'
    }

    /// Check if character is valid for an unquoted value
    fn is_valid_unquoted_value_char(c: char) -> bool {
        c.is_alphanumeric()
            || c == '_'
            || c == '-'
            || c == '/'
            || c == ':'
            || c == '.'
            || c == '*'
            || c == '?'
            || c == '!'
    }

    /// Check if a string is purely numeric
    fn is_purely_numeric(s: &str) -> bool {
        !s.is_empty() && s.chars().all(|c| c.is_ascii_digit())
    }

    /// Check if a value needs quoting for serialization
    fn needs_quoting(value: &str) -> bool {
        value.chars().any(|c| {
            c == ';' || c == '=' || c == '"' || c == '\\' || c == ' ' || c.is_uppercase()
        })
    }

    /// Quote a value for serialization
    fn quote_value(value: &str) -> String {
        let mut result = String::with_capacity(value.len() + 2);
        result.push('"');
        for c in value.chars() {
            if c == '"' || c == '\\' {
                result.push('\\');
            }
            result.push(c);
        }
        result.push('"');
        result
    }

    /// Get the canonical string representation of this tagged URN
    ///
    /// Uses the stored prefix
    /// Tags are already sorted alphabetically due to BTreeMap
    /// No trailing semicolon in canonical form
    /// Values are quoted only when necessary (smart quoting)
    /// Special value serialization:
    /// - `*` (must-have-any): serialized as value-less tag (just the key)
    /// - `?` (unspecified): serialized as key=?
    /// - `!` (must-not-have): serialized as key=!
    /// Serialize just the tags portion (without prefix)
    ///
    /// Returns the tags in canonical form with proper quoting and sorting.
    /// This is the portion after the ":" in a full URN string.
    ///
    /// Canonical serialization per stored value:
    ///
    /// | Stored value | Emitted             | Form                          |
    /// |--------------|---------------------|-------------------------------|
    /// | `"*"`        | `k`                 | bare key (must-have-any)      |
    /// | `"?"`        | `?k`                | prefix qualifier (no constraint) |
    /// | `"!"`        | `!k`                | prefix qualifier (must-not-have) |
    /// | `"?=v"`      | `k?=v`              | infix qualifier (absent or not v) |
    /// | `"!=v"`      | `k!=v`              | infix qualifier (present and not v) |
    /// | other `v`    | `k=v` or `k="v"`    | exact value (with quoting if needed) |
    ///
    /// Note that the prefix forms (`?k`, `!k`) and infix forms
    /// (`k?=v`, `k!=v`) are the canonical outputs even when the
    /// authored input used the alternative shape (`k?`, `k!`,
    /// `?k=v`, `!k=v`). The parser collapses all aliases to the
    /// single stored form; serialization emits the canonical
    /// representative deterministically.
    pub fn tags_to_string(&self) -> String {
        self
            .tags
            .iter()
            .map(|(k, v)| {
                match v.as_str() {
                    "*" => k.clone(),                       // bare key
                    "?" => format!("?{}", k),               // prefix `?k`
                    "!" => format!("!{}", k),               // prefix `!k`
                    qv if qv.starts_with("?=") => {
                        let raw = &qv[2..];
                        if Self::needs_quoting(raw) {
                            format!("{}?={}", k, Self::quote_value(raw))
                        } else {
                            format!("{}?={}", k, raw)
                        }
                    }
                    qv if qv.starts_with("!=") => {
                        let raw = &qv[2..];
                        if Self::needs_quoting(raw) {
                            format!("{}!={}", k, Self::quote_value(raw))
                        } else {
                            format!("{}!={}", k, raw)
                        }
                    }
                    _ if Self::needs_quoting(v) => format!("{}={}", k, Self::quote_value(v)),
                    _ => format!("{}={}", k, v),
                }
            })
            .collect::<Vec<_>>()
            .join(";")
    }

    pub fn to_string(&self) -> String {
        let tags_str = self.tags_to_string();
        format!("{}:{}", self.prefix, tags_str)
    }

    /// Get the prefix of this tagged URN
    pub fn get_prefix(&self) -> &str {
        &self.prefix
    }

    /// Get a specific tag value
    /// Key is normalized to lowercase for lookup
    pub fn get_tag(&self, key: &str) -> Option<&String> {
        self.tags.get(&key.to_lowercase())
    }

    /// Check if this URN has a specific tag with a specific value
    /// Key is normalized to lowercase; value comparison is case-sensitive
    pub fn has_tag(&self, key: &str, value: &str) -> bool {
        self.tags
            .get(&key.to_lowercase())
            .map_or(false, |v| v == value)
    }

    /// Check if a marker tag (a tag whose value is `*`) is present at the
    /// given key. Equivalent to `has_tag(tag_name, "*")` but expresses
    /// authorial intent: this tag is present as a marker (a wildcard-valued
    /// tag that serializes as just the key), not as a key=value pair.
    /// Example: `cap:constrained;...` has marker tag "constrained".
    pub fn has_marker_tag(&self, tag_name: &str) -> bool {
        self.tags
            .get(&tag_name.to_lowercase())
            .map_or(false, |v| v == "*")
    }

    /// Add or update a tag
    /// Key is normalized to lowercase; value is preserved as-is
    /// Returns error if value is empty (use "*" for wildcard)
    pub fn with_tag(mut self, key: String, value: String) -> Result<Self, TaggedUrnError> {
        if value.is_empty() {
            return Err(TaggedUrnError::EmptyTagComponent(format!(
                "empty value for key '{}' (use '*' for wildcard)",
                key
            )));
        }
        self.tags.insert(key.to_lowercase(), value);
        Ok(self)
    }

    /// Add or update a tag (infallible version for internal use where value is known valid)
    fn with_tag_unchecked(mut self, key: String, value: String) -> Self {
        self.tags.insert(key.to_lowercase(), value);
        self
    }

    /// Remove a tag
    /// Key is normalized to lowercase for case-insensitive removal
    pub fn without_tag(mut self, key: &str) -> Self {
        self.tags.remove(&key.to_lowercase());
        self
    }

    /// Check if this URN (instance) matches a pattern based on tag compatibility
    ///
    /// IMPORTANT: Both URNs must have the same prefix. Comparing URNs with
    /// different prefixes is a programming error and will return an error.
    ///
    /// Per-tag matching semantics:
    /// | Pattern Form | Interpretation              | Instance Missing | Instance = v | Instance = x≠v |
    /// |--------------|-----------------------------|--------------------|--------------|----------------|
    /// | (no entry)   | no constraint               | OK match           | OK match     | OK match       |
    /// | `K=?`        | no constraint (explicit)    | OK                 | OK           | OK             |
    /// | `K=!`        | **must-not-have**           | OK                 | NO           | NO             |
    /// | `K=*`        | **must-have, any value**    | NO                 | OK           | OK             |
    /// | `K=v`        | **must-have, exact value**  | NO                 | OK           | NO             |
    ///
    /// Special values work symmetrically on both instance and pattern sides.
    ///
    /// `self` is the instance, `pattern` is the pattern whose constraints must be satisfied.
    /// Equivalent to `pattern.accepts(self)`.
    pub fn conforms_to(&self, pattern: &TaggedUrn) -> Result<bool, TaggedUrnError> {
        Self::check_match(&self.tags, &self.prefix, &pattern.tags, &pattern.prefix)
    }

    /// Check if this URN (as a pattern) accepts the given instance.
    ///
    /// `self` is the pattern defining constraints, `instance` is tested against them.
    /// Equivalent to `instance.conforms_to(self)`.
    pub fn accepts(&self, instance: &TaggedUrn) -> Result<bool, TaggedUrnError> {
        Self::check_match(&instance.tags, &instance.prefix, &self.tags, &self.prefix)
    }

    /// Core matching: does `instance` satisfy `pattern`'s constraints?
    fn check_match(
        instance_tags: &BTreeMap<String, String>,
        instance_prefix: &str,
        pattern_tags: &BTreeMap<String, String>,
        pattern_prefix: &str,
    ) -> Result<bool, TaggedUrnError> {
        if instance_prefix != pattern_prefix {
            return Err(TaggedUrnError::PrefixMismatch {
                expected: pattern_prefix.to_string(),
                actual: instance_prefix.to_string(),
            });
        }

        let all_keys: std::collections::HashSet<&String> = instance_tags.keys()
            .chain(pattern_tags.keys())
            .collect();

        for key in all_keys {
            let inst = instance_tags.get(key).map(|s| s.as_str());
            let patt = pattern_tags.get(key).map(|s| s.as_str());

            if !Self::values_match(inst, patt) {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// One of the six canonical constraint forms a tag value can take,
    /// plus the implicit "missing" form (no entry in the tag map).
    /// The tag-storage strings map to these forms as follows:
    ///
    ///   None entry           -> Form::Missing
    ///   "?"                  -> Form::NoConstraint
    ///   "?=v"                -> Form::AbsentOrNotValue(v)
    ///   "*"                  -> Form::MustHaveAny
    ///   "!=v"                -> Form::PresentNotValue(v)
    ///   "!"                  -> Form::MustNotHave
    ///   exact `v`            -> Form::Exact(v)  (where v ∉ {"?", "!", "*"} and
    ///                                            v does not start with "?=" or "!=")
    fn classify_form(value: Option<&str>) -> Form<'_> {
        match value {
            None => Form::Missing,
            Some("?") => Form::NoConstraint,
            Some("*") => Form::MustHaveAny,
            Some("!") => Form::MustNotHave,
            Some(v) if v.starts_with("?=") => Form::AbsentOrNotValue(&v[2..]),
            Some(v) if v.starts_with("!=") => Form::PresentNotValue(&v[2..]),
            Some(v) => Form::Exact(v),
        }
    }

    /// Check if instance value matches pattern constraint, per the
    /// truth table over the six canonical forms.
    ///
    /// Full cross-product (instance row × pattern column):
    ///
    /// | Inst ↓ \ Pat → | Missing | `?` | `?=p` | `*` | `!=p` | `q` (exact) | `!` |
    /// |----------------|:-------:|:---:|:-----:|:---:|:-----:|:-----------:|:---:|
    /// | Missing        | ✓       | ✓   | ✓     | ✗   | ✗     | ✗           | ✓   |
    /// | `?`            | ✓       | ✓   | ✓     | ✓   | ✓     | ✓           | ✓   |
    /// | `?=v`          | ✓       | ✓   | ✓     | ✗   | ✗     | ✗           | ✓   |
    /// | `*`            | ✓       | ✓   | ✓     | ✓   | ✓     | ✓           | ✗   |
    /// | `!=v`          | ✓       | ✓   | ✓     | ✓   | ✓     | v==q ? ✗ : ✓| ✗   |
    /// | `q` (exact)    | ✓       | ✓   | q==p ? ✗ : ✓ | ✓ | q==p ? ✗ : ✓ | q==p ? ✓ : ✗ | ✗ |
    /// | `!`            | ✓       | ✓   | ✓     | ✗   | ✗     | ✗           | ✓   |
    ///
    /// Reading rules:
    /// - `?` (instance OR pattern) means "no constraint, anything goes."
    /// - `?=v` means "absent OR (present and not v)" — permissive.
    /// - `*` means "present with any value" — instance defers value
    ///   identity to runtime; pattern admits any value.
    /// - `!=v` means "present and not v" — strict presence with one
    ///   forbidden value.
    /// - exact `v` means "present and exactly v."
    /// - `!` means "absent" (must-not-have any value at this key).
    /// - `Missing` is symmetric to `?`: the key contributes no
    ///   constraint when not present in the URN.
    ///
    /// Cells where the pattern uses `?=p`, `!=p`, or exact `q` and
    /// the instance is also a value-bearing form: the result depends
    /// on whether the values overlap. The instance-`*` and
    /// pattern-side cases defer to runtime (instance has not pinned
    /// itself; pattern is permissive enough to accept some run).
    /// Per-key truth-table cell evaluation for the six canonical
    /// constraint forms (plus implicit Missing). Both arguments are
    /// stored tag values (or `None` to mean "key absent"). Returns
    /// true iff the instance value satisfies the pattern's
    /// constraint at this key.
    ///
    /// Exposed for callers (e.g. CapUrn's y-axis matcher) that walk
    /// tag sets themselves and need the same per-cell decision the
    /// tagged-URN matcher uses internally.
    pub fn values_match(inst: Option<&str>, patt: Option<&str>) -> bool {
        let i = Self::classify_form(inst);
        let p = Self::classify_form(patt);

        match (i, p) {
            // Pattern is unconditionally permissive on this key.
            (_, Form::Missing) | (_, Form::NoConstraint) => true,

            // Instance is unconditionally permissive — defers to
            // pattern entirely.
            (Form::NoConstraint, _) => true,

            // Pattern requires absent.
            (Form::Missing, Form::MustNotHave) => true,
            (Form::MustNotHave, Form::MustNotHave) => true,
            (Form::AbsentOrNotValue(_), Form::MustNotHave) => true, // absent satisfies
            (_, Form::MustNotHave) => false,                        // any presence fails

            // Pattern requires present (any).
            (Form::Missing, Form::MustHaveAny) => false,
            (Form::AbsentOrNotValue(_), Form::MustHaveAny) => false, // may be absent
            (Form::MustNotHave, Form::MustHaveAny) => false,
            (_, Form::MustHaveAny) => true,                          // *, !=v, exact, ?

            // Pattern: present-and-not-p.
            (Form::Missing, Form::PresentNotValue(_)) => false,
            (Form::AbsentOrNotValue(_), Form::PresentNotValue(_)) => false, // may be absent
            (Form::MustNotHave, Form::PresentNotValue(_)) => false,
            (Form::MustHaveAny, Form::PresentNotValue(_)) => true, // defer
            (Form::PresentNotValue(_), Form::PresentNotValue(_)) => true, // defer
            (Form::Exact(q), Form::PresentNotValue(p)) => q != p,

            // Pattern: absent-or-not-p.
            (Form::Missing, Form::AbsentOrNotValue(_)) => true,
            (Form::AbsentOrNotValue(_), Form::AbsentOrNotValue(_)) => true,
            (Form::MustNotHave, Form::AbsentOrNotValue(_)) => true,
            (Form::MustHaveAny, Form::AbsentOrNotValue(_)) => true, // defer
            (Form::PresentNotValue(_), Form::AbsentOrNotValue(_)) => true, // defer
            (Form::Exact(q), Form::AbsentOrNotValue(p)) => q != p,

            // Pattern: exact q.
            (Form::Missing, Form::Exact(_)) => false,
            (Form::AbsentOrNotValue(_), Form::Exact(_)) => false,
            (Form::MustNotHave, Form::Exact(_)) => false,
            (Form::MustHaveAny, Form::Exact(_)) => true, // defer
            (Form::PresentNotValue(v), Form::Exact(q)) => v != q,
            (Form::Exact(q), Form::Exact(p)) => q == p,
        }
    }

    pub fn conforms_to_str(&self, pattern_str: &str) -> Result<bool, TaggedUrnError> {
        let pattern = TaggedUrn::from_string(pattern_str)?;
        self.conforms_to(&pattern)
    }

    pub fn accepts_str(&self, instance_str: &str) -> Result<bool, TaggedUrnError> {
        let instance = TaggedUrn::from_string(instance_str)?;
        self.accepts(&instance)
    }

    /// Check if two URNs are equivalent (identical tag sets).
    ///
    /// From order theory: in the specialization partial order defined by
    /// `accepts`/`conforms_to`, two elements are **equivalent** when each
    /// accepts the other (antisymmetry: a ≤ b ∧ b ≤ a → a = b).
    ///
    /// This is stricter than `is_comparable` — it requires the tag sets to
    /// be identical, not just related by specialization.
    ///
    /// ```text
    /// a.is_equivalent(&b)  ≡  a.accepts(&b) && b.accepts(&a)
    /// ```
    ///
    /// Returns `PrefixMismatch` error if prefixes differ (inherited from
    /// `accepts`/`conforms_to` — both sides return false on mismatch, but
    /// since we AND them, the error propagates).
    pub fn is_equivalent(&self, other: &TaggedUrn) -> Result<bool, TaggedUrnError> {
        Ok(self.accepts(other)? && other.accepts(self)?)
    }

    /// Check if two URNs are comparable (one is a specialization of the other).
    ///
    /// From order theory: in a partial order, two elements are **comparable**
    /// when one is ≤ the other. Elements that are NOT comparable are in
    /// different branches of the specialization lattice (e.g., `media:pdf`
    /// vs `media:txt;textable` — neither accepts the other).
    ///
    /// This is the weakest relation: it finds all URNs on the same
    /// generalization/specialization chain. Use it when you want to discover
    /// all handlers that *could* service a request, whether they are more
    /// general (fallback) or more specific (exact match).
    ///
    /// ```text
    /// a.is_comparable(&b)  ≡  a.accepts(&b) || b.accepts(&a)
    /// ```
    ///
    /// Returns `PrefixMismatch` error if prefixes differ (inherited from
    /// `accepts`/`conforms_to`).
    pub fn is_comparable(&self, other: &TaggedUrn) -> Result<bool, TaggedUrnError> {
        Ok(self.accepts(other)? || other.accepts(self)?)
    }

    /// String variant of `is_equivalent`.
    pub fn is_equivalent_str(&self, other_str: &str) -> Result<bool, TaggedUrnError> {
        let other = TaggedUrn::from_string(other_str)?;
        self.is_equivalent(&other)
    }

    /// String variant of `is_comparable`.
    pub fn is_comparable_str(&self, other_str: &str) -> Result<bool, TaggedUrnError> {
        let other = TaggedUrn::from_string(other_str)?;
        self.is_comparable(&other)
    }

    /// Calculate specificity score for URN matching
    ///
    /// Calculate specificity score: sum of per-tag truth-table scores.
    ///
    /// Graded scoring per the canonical form ladder:
    ///
    /// | Stored value | Form           | Score |
    /// |--------------|----------------|------:|
    /// | `"?"`        | `?x`           |     0 |
    /// | `"?=v"`      | `x?=v`         |     1 |
    /// | `"*"`        | `x` (`x=*`)    |     2 |
    /// | `"!=v"`      | `x!=v`         |     3 |
    /// | exact `v`    | `x=v`          |     4 |
    /// | `"!"`        | `!x`           |     5 |
    ///
    /// Higher scores indicate more constrained (more specific) tags.
    /// Identity (`prefix:` with no tags) scores 0. The ladder is
    /// monotone within each "branch": `?x` (0) → `x?=v` (1) → `x` (2)
    /// → exact `x=v` (4) tightens positively; `?x` (0) → `x?=v` (1)
    /// → `x!=v` (3) → `!x` (5) tightens negatively.
    pub fn specificity(&self) -> usize {
        self.tags.values().map(|v| score_tag_value(v.as_str())).sum()
    }

    /// Get specificity as a tuple for tie-breaking. Counts how many
    /// tags fall into each non-zero form bucket. Compare tuples
    /// lexicographically when sum scores are equal.
    ///
    /// Returns `(exact, present_not_value, must_have_any, present_not_value_count, absent_or_not_value, must_not_have)` —
    /// ordered from highest score to lowest, so a lex-greater tuple
    /// means a denser concentration of high-specificity tags.
    pub fn specificity_tuple(&self) -> (usize, usize, usize, usize, usize) {
        let mut must_not_have = 0;
        let mut exact = 0;
        let mut present_not_value = 0;
        let mut must_have_any = 0;
        let mut absent_or_not_value = 0;
        for v in self.tags.values() {
            match Self::classify_form(Some(v.as_str())) {
                Form::MustNotHave => must_not_have += 1,
                Form::Exact(_) => exact += 1,
                Form::PresentNotValue(_) => present_not_value += 1,
                Form::MustHaveAny => must_have_any += 1,
                Form::AbsentOrNotValue(_) => absent_or_not_value += 1,
                Form::NoConstraint | Form::Missing => {}
            }
        }
        (
            must_not_have,
            exact,
            present_not_value,
            must_have_any,
            absent_or_not_value,
        )
    }

    /// Check if this URN is more specific than another
    ///
    /// Compares specificity scores after verifying same prefix.
    /// Only meaningful when both patterns already matched the same request.
    pub fn is_more_specific_than(&self, other: &TaggedUrn) -> Result<bool, TaggedUrnError> {
        if self.prefix != other.prefix {
            return Err(TaggedUrnError::PrefixMismatch {
                expected: self.prefix.clone(),
                actual: other.prefix.clone(),
            });
        }

        Ok(self.specificity() > other.specificity())
    }


    /// Create a wildcard version by replacing specific values with wildcards
    pub fn with_wildcard_tag(self, key: &str) -> Self {
        if self.tags.contains_key(key) {
            self.with_tag_unchecked(key.to_string(), "*".to_string())
        } else {
            self
        }
    }

    /// Create a subset URN with only specified tags
    pub fn subset(&self, keys: &[&str]) -> Self {
        let mut tags = BTreeMap::new();
        for &key in keys {
            if let Some(value) = self.tags.get(key) {
                tags.insert(key.to_string(), value.clone());
            }
        }
        Self {
            prefix: self.prefix.clone(),
            tags,
        }
    }

    /// Merge with another URN (other takes precedence for conflicts)
    /// Both must have the same prefix
    pub fn merge(&self, other: &TaggedUrn) -> Result<Self, TaggedUrnError> {
        if self.prefix != other.prefix {
            return Err(TaggedUrnError::PrefixMismatch {
                expected: self.prefix.clone(),
                actual: other.prefix.clone(),
            });
        }

        let mut tags = self.tags.clone();
        for (key, value) in &other.tags {
            tags.insert(key.clone(), value.clone());
        }
        Ok(Self {
            prefix: self.prefix.clone(),
            tags,
        })
    }

    pub fn canonical(tagged_urn: &str) -> Result<String, TaggedUrnError> {
        let tagged_urn_deserialized = TaggedUrn::from_string(tagged_urn)?;
        Ok(tagged_urn_deserialized.to_string())
    }

    pub fn canonical_option(tagged_urn: Option<&str>) -> Result<Option<String>, TaggedUrnError> {
        if let Some(cu) = tagged_urn {
            let tagged_urn_deserialized = TaggedUrn::from_string(cu)?;
            Ok(Some(tagged_urn_deserialized.to_string()))
        } else {
            Ok(None)
        }
    }
}

/// Errors that can occur when parsing or operating on tagged URNs
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TaggedUrnError {
    /// Error code 1: Empty or malformed URN
    Empty,
    /// Error code 5: URN does not have a prefix (no colon found)
    MissingPrefix,
    /// Error code 10: Empty prefix (colon at start)
    EmptyPrefix,
    /// Error code 4: Tag not in key=value format
    InvalidTagFormat(String),
    /// Error code 2: Empty key or value component
    EmptyTagComponent(String),
    /// Error code 3: Disallowed character in key/value
    InvalidCharacter(String),
    /// Error code 6: Same key appears twice
    DuplicateKey(String),
    /// Error code 7: Key is purely numeric
    NumericKey(String),
    /// Error code 8: Quoted value never closed
    UnterminatedQuote(usize),
    /// Error code 9: Invalid escape in quoted value (only \" and \\ allowed)
    InvalidEscapeSequence(usize),
    /// Error code 11: Prefix mismatch when comparing URNs from different domains
    PrefixMismatch { expected: String, actual: String },
    /// Error code 12: Input has leading or trailing whitespace
    WhitespaceInInput(String),
}

impl fmt::Display for TaggedUrnError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TaggedUrnError::Empty => {
                write!(f, "Tagged URN cannot be empty")
            }
            TaggedUrnError::MissingPrefix => {
                write!(f, "Tagged URN must have a prefix followed by ':'")
            }
            TaggedUrnError::EmptyPrefix => {
                write!(f, "Tagged URN prefix cannot be empty")
            }
            TaggedUrnError::InvalidTagFormat(tag) => {
                write!(f, "Invalid tag format (must be key=value): {}", tag)
            }
            TaggedUrnError::EmptyTagComponent(tag) => {
                write!(f, "Tag key or value cannot be empty: {}", tag)
            }
            TaggedUrnError::InvalidCharacter(tag) => {
                write!(f, "Invalid character in tag: {}", tag)
            }
            TaggedUrnError::DuplicateKey(key) => {
                write!(f, "Duplicate tag key: {}", key)
            }
            TaggedUrnError::NumericKey(key) => {
                write!(f, "Tag key cannot be purely numeric: {}", key)
            }
            TaggedUrnError::UnterminatedQuote(pos) => {
                write!(f, "Unterminated quote at position {}", pos)
            }
            TaggedUrnError::InvalidEscapeSequence(pos) => {
                write!(
                    f,
                    "Invalid escape sequence at position {} (only \\\" and \\\\ allowed)",
                    pos
                )
            }
            TaggedUrnError::PrefixMismatch { expected, actual } => {
                write!(
                    f,
                    "Cannot compare URNs with different prefixes: '{}' vs '{}'",
                    expected, actual
                )
            }
            TaggedUrnError::WhitespaceInInput(input) => {
                write!(
                    f,
                    "Tagged URN has leading or trailing whitespace: '{}'",
                    input
                )
            }
        }
    }
}

impl std::error::Error for TaggedUrnError {}

impl FromStr for TaggedUrn {
    type Err = TaggedUrnError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        TaggedUrn::from_string(s)
    }
}

impl fmt::Display for TaggedUrn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

// Serde serialization support
impl Serialize for TaggedUrn {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for TaggedUrn {
    fn deserialize<D>(deserializer: D) -> Result<TaggedUrn, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        TaggedUrn::from_string(&s).map_err(serde::de::Error::custom)
    }
}

/// URN matching and selection utilities
pub struct UrnMatcher;

impl UrnMatcher {
    /// Find the most specific URN that conforms to a request's constraints.
    /// URNs are instances (capabilities), request is the pattern (requirement).
    /// All URNs must have the same prefix as the request.
    pub fn find_best_match<'a>(urns: &'a [TaggedUrn], request: &TaggedUrn) -> Result<Option<&'a TaggedUrn>, TaggedUrnError> {
        let mut best: Option<&TaggedUrn> = None;
        let mut best_specificity = 0;

        for urn in urns {
            if urn.conforms_to(request)? {
                let specificity = urn.specificity();
                if best.is_none() || specificity > best_specificity {
                    best = Some(urn);
                    best_specificity = specificity;
                }
            }
        }

        Ok(best)
    }

    /// Find all URNs that conform to a request's constraints, sorted by specificity.
    /// URNs are instances (capabilities), request is the pattern (requirement).
    /// All URNs must have the same prefix as the request.
    pub fn find_all_matches<'a>(urns: &'a [TaggedUrn], request: &TaggedUrn) -> Result<Vec<&'a TaggedUrn>, TaggedUrnError> {
        let mut results: Vec<&TaggedUrn> = Vec::new();

        for urn in urns {
            if urn.conforms_to(request)? {
                results.push(urn);
            }
        }

        // Sort by specificity (most specific first)
        results.sort_by_key(|urn| std::cmp::Reverse(urn.specificity()));
        Ok(results)
    }

    /// Check if two URN sets are compatible
    /// All URNs in both sets must have the same prefix
    pub fn are_compatible(urns1: &[TaggedUrn], urns2: &[TaggedUrn]) -> Result<bool, TaggedUrnError> {
        for u1 in urns1 {
            for u2 in urns2 {
                if u1.accepts(u2)? || u2.accepts(u1)? {
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }
}

/// Builder for creating tagged URNs fluently
pub struct TaggedUrnBuilder {
    prefix: String,
    tags: BTreeMap<String, String>,
}

impl TaggedUrnBuilder {
    /// Create a new builder with a specified prefix (required)
    pub fn new(prefix: &str) -> Self {
        Self {
            prefix: prefix.to_lowercase(),
            tags: BTreeMap::new(),
        }
    }

    /// Add a tag with key (normalized to lowercase) and value (preserved as-is)
    /// Returns error if value is empty (use "*" for wildcard)
    pub fn tag(mut self, key: &str, value: &str) -> Result<Self, TaggedUrnError> {
        if value.is_empty() {
            return Err(TaggedUrnError::EmptyTagComponent(format!(
                "empty value for key '{}' (use '*' for wildcard)",
                key
            )));
        }
        self.tags.insert(key.to_lowercase(), value.to_string());
        Ok(self)
    }

	/// Add a tag with key (normalized to lowercase) and wildcard value
    pub fn marker(mut self, key: &str) -> Self {
        self.tags.insert(key.to_lowercase(), "*".to_string());
        self
    }

    pub fn build(self) -> Result<TaggedUrn, TaggedUrnError> {
        if self.tags.is_empty() {
            return Err(TaggedUrnError::Empty);
        }
        Ok(TaggedUrn {
            prefix: self.prefix,
            tags: self.tags,
        })
    }

    /// Build allowing empty tags (creates an empty URN that matches everything)
    pub fn build_allow_empty(self) -> TaggedUrn {
        TaggedUrn {
            prefix: self.prefix,
            tags: self.tags,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // TEST501: Create tagged URN from string and verify prefix and tag values
    #[test]
    fn test_tagged_urn_creation() {
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail;").unwrap();
        assert_eq!(urn.get_prefix(), "cap");
        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.get_tag("target"), Some(&"thumbnail".to_string()));
        assert_eq!(urn.get_tag("ext"), Some(&"pdf".to_string()));
    }

    // TEST502: Parse URN with custom prefix and verify serialization
    #[test]
    fn test_custom_prefix() {
        let urn = TaggedUrn::from_string("myapp:generate;ext=pdf").unwrap();
        assert_eq!(urn.get_prefix(), "myapp");
        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.to_string(), "myapp:ext=pdf;generate");
    }

    // TEST503: Normalize prefix to lowercase regardless of input case
    #[test]
    fn test_prefix_case_insensitive() {
        // Three URNs differing only in prefix case (CAP, cap, Cap) — all
        // must normalize to the same `cap` prefix and be equal once
        // parsed. Tag content is identical across all three.
        let urn1 = TaggedUrn::from_string("CAP:test").unwrap();
        let urn2 = TaggedUrn::from_string("cap:test").unwrap();
        let urn3 = TaggedUrn::from_string("Cap:test").unwrap();

        assert_eq!(urn1.get_prefix(), "cap");
        assert_eq!(urn2.get_prefix(), "cap");
        assert_eq!(urn3.get_prefix(), "cap");
        assert_eq!(urn1, urn2);
        assert_eq!(urn2, urn3);
    }

    // TEST504: Return PrefixMismatch error when comparing URNs with different prefixes
    #[test]
    fn test_prefix_mismatch_error() {
        let urn1 = TaggedUrn::from_string("cap:in=media:;out=media:;test").unwrap();
        let urn2 = TaggedUrn::from_string("myapp:test").unwrap();

        // urn1 (cap) is instance, urn2 (myapp) is pattern
        // expected = pattern prefix, actual = instance prefix
        let result = urn1.conforms_to(&urn2);
        assert!(result.is_err());
        if let Err(TaggedUrnError::PrefixMismatch { expected, actual }) = result {
            assert_eq!(expected, "myapp");
            assert_eq!(actual, "cap");
        } else {
            panic!("Expected PrefixMismatch error");
        }
    }

    // TEST505: Build URN with custom prefix using TaggedUrnBuilder
    #[test]
    fn test_builder_with_prefix() {
        let urn = TaggedUrnBuilder::new("custom")
            .tag("key", "value").unwrap()
            .build()
            .unwrap();

        assert_eq!(urn.get_prefix(), "custom");
        assert_eq!(urn.to_string(), "custom:key=value");
    }

    // TEST506: Normalize unquoted keys and values to lowercase
    #[test]
    fn test_unquoted_values_lowercased() {
        // Unquoted values are normalized to lowercase
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail;").unwrap();

        // Keys are always lowercase
        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.get_tag("ext"), Some(&"pdf".to_string()));
        assert_eq!(urn.get_tag("target"), Some(&"thumbnail".to_string()));

        // Key lookup is case-insensitive (try uppercase variations of an
        // existing key — `EXT` and `Ext` resolve to the same `ext` value).
        assert_eq!(urn.get_tag("EXT"), Some(&"pdf".to_string()));
        assert_eq!(urn.get_tag("Ext"), Some(&"pdf".to_string()));

        // Both URNs parse to same lowercase values (same tags, same values)
        let urn2 = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail;").unwrap();
        assert_eq!(urn.to_string(), urn2.to_string());
        assert_eq!(urn, urn2);
    }

    // TEST507: Preserve original case for quoted values while lowercasing keys
    #[test]
    fn test_quoted_values_preserve_case() {
        // Quoted values preserve their case
        let urn = TaggedUrn::from_string(r#"cap:key="Value With Spaces""#).unwrap();
        assert_eq!(urn.get_tag("key"), Some(&"Value With Spaces".to_string()));

        // Key is still lowercase
        let urn2 = TaggedUrn::from_string(r#"cap:KEY="Value With Spaces""#).unwrap();
        assert_eq!(urn2.get_tag("key"), Some(&"Value With Spaces".to_string()));

        // Unquoted vs quoted case difference
        let unquoted = TaggedUrn::from_string("cap:key=UPPERCASE").unwrap();
        let quoted = TaggedUrn::from_string(r#"cap:key="UPPERCASE""#).unwrap();
        assert_eq!(unquoted.get_tag("key"), Some(&"uppercase".to_string())); // lowercase
        assert_eq!(quoted.get_tag("key"), Some(&"UPPERCASE".to_string())); // preserved
        assert_ne!(unquoted, quoted); // NOT equal
    }

    // TEST508: Parse quoted values containing semicolons, equals signs, and spaces
    #[test]
    fn test_quoted_value_special_chars() {
        // Semicolons in quoted values
        let urn = TaggedUrn::from_string(r#"cap:key="value;with;semicolons""#).unwrap();
        assert_eq!(urn.get_tag("key"), Some(&"value;with;semicolons".to_string()));

        // Equals in quoted values
        let urn2 = TaggedUrn::from_string(r#"cap:key="value=with=equals""#).unwrap();
        assert_eq!(urn2.get_tag("key"), Some(&"value=with=equals".to_string()));

        // Spaces in quoted values
        let urn3 = TaggedUrn::from_string(r#"cap:key="hello world""#).unwrap();
        assert_eq!(urn3.get_tag("key"), Some(&"hello world".to_string()));
    }

    // TEST509: Parse escape sequences for quotes and backslashes in quoted values
    #[test]
    fn test_quoted_value_escape_sequences() {
        // Escaped quotes
        let urn = TaggedUrn::from_string(r#"cap:key="value\"quoted\"""#).unwrap();
        assert_eq!(urn.get_tag("key"), Some(&r#"value"quoted""#.to_string()));

        // Escaped backslashes
        let urn2 = TaggedUrn::from_string(r#"cap:key="path\\file""#).unwrap();
        assert_eq!(urn2.get_tag("key"), Some(&r#"path\file"#.to_string()));

        // Mixed escapes
        let urn3 = TaggedUrn::from_string(r#"cap:key="say \"hello\\world\"""#).unwrap();
        assert_eq!(urn3.get_tag("key"), Some(&r#"say "hello\world""#.to_string()));
    }

    // TEST510: Parse URN with both quoted and unquoted tag values
    #[test]
    fn test_mixed_quoted_unquoted() {
        let urn = TaggedUrn::from_string(r#"cap:a="Quoted";b=simple"#).unwrap();
        assert_eq!(urn.get_tag("a"), Some(&"Quoted".to_string()));
        assert_eq!(urn.get_tag("b"), Some(&"simple".to_string()));
    }

    // TEST511: Reject unterminated quoted value with appropriate error
    #[test]
    fn test_unterminated_quote_error() {
        let result = TaggedUrn::from_string(r#"cap:key="unterminated"#);
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(matches!(e, TaggedUrnError::UnterminatedQuote(_)));
        }
    }

    // TEST512: Reject invalid escape sequences in quoted values
    #[test]
    fn test_invalid_escape_sequence_error() {
        let result = TaggedUrn::from_string(r#"cap:key="bad\n""#);
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(matches!(e, TaggedUrnError::InvalidEscapeSequence(_)));
        }

        // Invalid escape at end
        let result2 = TaggedUrn::from_string(r#"cap:key="bad\x""#);
        assert!(result2.is_err());
        if let Err(e) = result2 {
            assert!(matches!(e, TaggedUrnError::InvalidEscapeSequence(_)));
        }
    }

    // TEST513: Apply smart quoting during serialization based on value content
    #[test]
    fn test_serialization_smart_quoting() {
        // Simple lowercase value - no quoting needed
        let urn = TaggedUrnBuilder::new("cap").tag("key", "simple").unwrap().build().unwrap();
        assert_eq!(urn.to_string(), "cap:key=simple");

        // Value with spaces - needs quoting
        let urn2 = TaggedUrnBuilder::new("cap")
            .tag("key", "has spaces").unwrap()
            .build()
            .unwrap();
        assert_eq!(urn2.to_string(), r#"cap:key="has spaces""#);

        // Value with semicolons - needs quoting
        let urn3 = TaggedUrnBuilder::new("cap")
            .tag("key", "has;semi").unwrap()
            .build()
            .unwrap();
        assert_eq!(urn3.to_string(), r#"cap:key="has;semi""#);

        // Value with uppercase - needs quoting to preserve
        let urn4 = TaggedUrnBuilder::new("cap")
            .tag("key", "HasUpper").unwrap()
            .build()
            .unwrap();
        assert_eq!(urn4.to_string(), r#"cap:key="HasUpper""#);

        // Value with quotes - needs quoting and escaping
        let urn5 = TaggedUrnBuilder::new("cap")
            .tag("key", r#"has"quote"#).unwrap()
            .build()
            .unwrap();
        assert_eq!(urn5.to_string(), r#"cap:key="has\"quote""#);

        // Value with backslashes - needs quoting and escaping
        let urn6 = TaggedUrnBuilder::new("cap")
            .tag("key", r#"path\file"#).unwrap()
            .build()
            .unwrap();
        assert_eq!(urn6.to_string(), r#"cap:key="path\\file""#);
    }

    // TEST514: Round-trip parse and serialize a simple URN
    #[test]
    fn test_round_trip_simple() {
        let original = "cap:ext=pdf;generate;in=media:;out=media:";
        let urn = TaggedUrn::from_string(original).unwrap();
        let serialized = urn.to_string();
        let reparsed = TaggedUrn::from_string(&serialized).unwrap();
        assert_eq!(urn, reparsed);
    }

    // TEST515: Round-trip parse and serialize a URN with quoted values
    #[test]
    fn test_round_trip_quoted() {
        let original = r#"cap:key="Value With Spaces""#;
        let urn = TaggedUrn::from_string(original).unwrap();
        let serialized = urn.to_string();
        let reparsed = TaggedUrn::from_string(&serialized).unwrap();
        assert_eq!(urn, reparsed);
        assert_eq!(reparsed.get_tag("key"), Some(&"Value With Spaces".to_string()));
    }

    // TEST516: Round-trip parse and serialize a URN with escape sequences
    #[test]
    fn test_round_trip_escapes() {
        let original = r#"cap:key="value\"with\\escapes""#;
        let urn = TaggedUrn::from_string(original).unwrap();
        assert_eq!(urn.get_tag("key"), Some(&r#"value"with\escapes"#.to_string()));
        let serialized = urn.to_string();
        let reparsed = TaggedUrn::from_string(&serialized).unwrap();
        assert_eq!(urn, reparsed);
    }

    // TEST517: Require a prefix in URN string and reject missing prefix
    #[test]
    fn test_prefix_required() {
        // Missing prefix should fail
        assert!(TaggedUrn::from_string("generate;ext=pdf").is_err());

        // Valid prefix should work
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        assert!(urn.has_marker_tag("generate"));

        // Case-insensitive prefix
        let urn2 = TaggedUrn::from_string("CAP:generate").unwrap();
        assert!(urn2.has_marker_tag("generate"));
    }

    // TEST518: Treat trailing semicolon as equivalent to no trailing semicolon
    #[test]
    fn test_trailing_semicolon_equivalence() {
        // Both with and without trailing semicolon should be equivalent
        let urn1 = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let urn2 = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;").unwrap();

        // They should be equal
        assert_eq!(urn1, urn2);

        // They should have same hash
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher1 = DefaultHasher::new();
        urn1.hash(&mut hasher1);
        let hash1 = hasher1.finish();

        let mut hasher2 = DefaultHasher::new();
        urn2.hash(&mut hasher2);
        let hash2 = hasher2.finish();

        assert_eq!(hash1, hash2);

        // They should have same string representation (canonical form)
        assert_eq!(urn1.to_string(), urn2.to_string());

        // They should match each other
        assert!(urn1.conforms_to(&urn2).unwrap());
        assert!(urn2.conforms_to(&urn1).unwrap());
    }

    // TEST519: Serialize tags in alphabetical order as canonical string format
    #[test]
    fn test_canonical_string_format() {
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail").unwrap();
        // Should be sorted alphabetically and have no trailing semicolon in canonical form
        // Alphabetical order: ext < op < target
        assert_eq!(
            urn.to_string(),
            "cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail"
        );
    }

    // TEST520: Match tags with exact values, subsets, wildcards, and mismatches
    #[test]
    fn test_tag_matching() {
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail;").unwrap();

        // Exact match
        let request1 =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail;").unwrap();
        assert!(urn.conforms_to(&request1).unwrap());

        // Subset match
        let request2 = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();
        assert!(urn.conforms_to(&request2).unwrap());

        // Wildcard request should match specific URN
        let request3 = TaggedUrn::from_string("cap:ext=*").unwrap();
        assert!(urn.conforms_to(&request3).unwrap()); // URN has ext=pdf, request accepts any ext

        // No match - conflicting value
        let request4 = TaggedUrn::from_string("cap:extract;in=media:;out=media:").unwrap();
        assert!(!urn.conforms_to(&request4).unwrap());
    }

    // TEST521: Enforce case-sensitive matching for quoted tag values
    #[test]
    fn test_matching_case_sensitive_values() {
        // Values with different case should NOT match
        let urn1 = TaggedUrn::from_string(r#"cap:key="Value""#).unwrap();
        let urn2 = TaggedUrn::from_string(r#"cap:key="value""#).unwrap();
        assert!(!urn1.conforms_to(&urn2).unwrap());
        assert!(!urn2.conforms_to(&urn1).unwrap());

        // Same case should match
        let urn3 = TaggedUrn::from_string(r#"cap:key="Value""#).unwrap();
        assert!(urn1.conforms_to(&urn3).unwrap());
    }

    // TEST522: Handle missing tags in instance vs pattern matching semantics
    #[test]
    fn test_missing_tag_handling() {
        // NEW SEMANTICS: Missing tag in instance means the tag doesn't exist.
        // Pattern constraints must be satisfied by instance.

        let urn = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();

        // Pattern with tag that instance doesn't have: NO MATCH
        // Pattern ext=pdf requires instance to have ext=pdf, but instance doesn't have ext
        let pattern1 = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        assert!(!urn.conforms_to(&pattern1).unwrap()); // Instance missing ext, pattern wants ext=pdf

        // Pattern missing tag = no constraint: MATCH
        // Instance has generate, pattern has no constraint on op
        let urn2 = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let pattern2 = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();
        assert!(urn2.conforms_to(&pattern2).unwrap()); // Instance has ext=pdf, pattern doesn't constrain ext

        // To match any value of a tag, use explicit ? or *
        let pattern3 = TaggedUrn::from_string("cap:ext=?").unwrap(); // ? = no constraint
        assert!(urn.conforms_to(&pattern3).unwrap()); // Instance missing ext, pattern doesn't care

        // * means must-have-any - instance must have the tag
        let pattern4 = TaggedUrn::from_string("cap:ext=*").unwrap();
        assert!(!urn.conforms_to(&pattern4).unwrap()); // Instance missing ext, pattern requires ext to be present
    }

    // TEST523: Compute graded specificity scores and tuples for URN tags
    #[test]
    fn test_specificity() {
        // Six-form per-tag specificity ladder:
        //   ?x        : 0  (no constraint)
        //   x?=v      : 1  (absent OR not v)
        //   x (=x=*)  : 2  (must-have-any)
        //   x!=v      : 3  (present and not v)
        //   x=v       : 4  (must-have-this-value)
        //   !x        : 5  (must-not-have)

        let urn1 = TaggedUrn::from_string("cap:general").unwrap();      // bare marker -> 2
        let urn2 = TaggedUrn::from_string("cap:ext=pdf").unwrap();      // exact -> 4
        let urn3 = TaggedUrn::from_string("cap:gen;ext=pdf").unwrap();  // marker + exact = 2+4
        let urn4 = TaggedUrn::from_string("cap:?ext").unwrap();         // ?x -> 0
        let urn5 = TaggedUrn::from_string("cap:!ext").unwrap();         // !x -> 5
        let urn6 = TaggedUrn::from_string("cap:ext?=pdf").unwrap();     // x?=v -> 1
        let urn7 = TaggedUrn::from_string("cap:ext!=pdf").unwrap();     // x!=v -> 3

        assert_eq!(urn1.specificity(), 2);
        assert_eq!(urn2.specificity(), 4);
        assert_eq!(urn3.specificity(), 6);
        assert_eq!(urn4.specificity(), 0);
        assert_eq!(urn5.specificity(), 5);
        assert_eq!(urn6.specificity(), 1);
        assert_eq!(urn7.specificity(), 3);

        // Five-tuple specificity for tie-breaking — counts of tags in
        // each non-zero form bucket: (must_not_have, exact,
        // present_not_value, must_have_any, absent_or_not_value).
        assert_eq!(urn2.specificity_tuple(), (0, 1, 0, 0, 0)); // 1 exact
        assert_eq!(urn3.specificity_tuple(), (0, 1, 0, 1, 0)); // 1 exact + 1 marker
        assert_eq!(urn5.specificity_tuple(), (1, 0, 0, 0, 0)); // 1 must-not-have

        assert!(urn2.is_more_specific_than(&urn1).unwrap()); // exact(4) > marker(2)
    }

    // TEST524: Build URN with multiple tags using TaggedUrnBuilder
    #[test]
    fn test_builder() {
        let urn = TaggedUrnBuilder::new("cap")
            .marker("generate")
            .tag("target", "thumbnail").unwrap()
            .tag("ext", "pdf").unwrap()
            .tag("output", "binary").unwrap()
            .build()
            .unwrap();

        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.get_tag("output"), Some(&"binary".to_string()));
    }

    // TEST525: Preserve value case in builder while lowercasing keys
    #[test]
    fn test_builder_preserves_case() {
        let urn = TaggedUrnBuilder::new("cap")
            .tag("KEY", "ValueWithCase").unwrap()
            .build()
            .unwrap();

        // Key is lowercase
        assert_eq!(urn.get_tag("key"), Some(&"ValueWithCase".to_string()));
        // Value case preserved, so needs quoting
        assert_eq!(urn.to_string(), r#"cap:key="ValueWithCase""#);
    }

    // TEST526: Verify directional accepts between patterns with shared and disjoint tags
    #[test]
    fn test_directional_accepts_with_tag_overlap() {
        let specific = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let general = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();
        let different = TaggedUrn::from_string("cap:extract;image;in=media:;out=media:").unwrap();
        let wildcard = TaggedUrn::from_string("cap:format;generate;in=media:;out=media:").unwrap();

        // General pattern accepts specific instance (missing ext in pattern = no constraint)
        assert!(general.accepts(&specific).unwrap());
        // Specific does NOT accept general (ext=pdf requires ext, general has none)
        assert!(!specific.accepts(&general).unwrap());

        // Different op values: neither direction accepts
        assert!(!specific.accepts(&different).unwrap());
        assert!(!different.accepts(&specific).unwrap());

        // Wildcard with format=* does NOT accept specific (specific has no format, * requires present)
        assert!(!wildcard.accepts(&specific).unwrap());
        // Specific does NOT accept wildcard (wildcard has no ext, specific requires ext=pdf)
        assert!(!specific.accepts(&wildcard).unwrap());

        // But a fully-specified instance satisfies both
        let full_instance = TaggedUrn::from_string("cap:ext=pdf;format=png;generate;in=media:;out=media:").unwrap();
        assert!(specific.accepts(&full_instance).unwrap());
        assert!(wildcard.accepts(&full_instance).unwrap());
    }

    // TEST527: Find best matching URN by specificity from a list of candidates
    #[test]
    fn test_best_match() {
        let urns = vec![
            TaggedUrn::from_string("cap:op").unwrap(),
            TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap(),
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap(),
        ];

        let request = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();
        let best = UrnMatcher::find_best_match(&urns, &request).unwrap().unwrap();

        // Most specific URN that can handle the request
        // Alphabetical order: ext < op
        assert_eq!(best.to_string(), "cap:ext=pdf;generate;in=media:;out=media:");
    }

    // TEST528: Merge two URNs and extract a subset of tags
    #[test]
    fn test_merge_and_subset() {
        let urn1 = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();
        let urn2 = TaggedUrn::from_string("cap:ext=pdf;output=binary").unwrap();

        let merged = urn1.merge(&urn2).unwrap();
        // Alphabetical order: ext < op < output
        assert_eq!(
            merged.to_string(),
            "cap:ext=pdf;generate;in=media:;out=media:;output=binary"
        );

        let subset = merged.subset(&["type", "ext"]);
        assert_eq!(subset.to_string(), "cap:ext=pdf");
    }

    // TEST529: Reject merge of URNs with different prefixes
    #[test]
    fn test_merge_prefix_mismatch() {
        let urn1 = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();
        let urn2 = TaggedUrn::from_string("myapp:ext=pdf").unwrap();

        let result = urn1.merge(&urn2);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), TaggedUrnError::PrefixMismatch { .. }));
    }

    // TEST530: Convert specific tag value to wildcard and verify matching behavior
    #[test]
    fn test_wildcard_tag() {
        let urn = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        let wildcarded = urn.clone().with_wildcard_tag("ext");

        // Wildcard serializes as value-less tag
        assert_eq!(wildcarded.to_string(), "cap:ext");

        // Test that wildcarded URN can match more requests
        let request = TaggedUrn::from_string("cap:ext=jpg").unwrap();
        assert!(!urn.conforms_to(&request).unwrap());
        assert!(wildcarded.conforms_to(&TaggedUrn::from_string("cap:ext").unwrap()).unwrap());
    }

    // TEST531: Handle empty tagged URN with no tags in matching and serialization
    #[test]
    fn test_empty_tagged_urn() {
        // Empty tagged URN is valid
        let empty_urn = TaggedUrn::from_string("cap:").unwrap();
        assert_eq!(empty_urn.tags.len(), 0);
        assert_eq!(empty_urn.to_string(), "cap:");

        // NEW SEMANTICS:
        // Empty PATTERN matches any INSTANCE (pattern has no constraints)
        // Empty INSTANCE only matches patterns that have no required tags

        let specific_urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();

        // Empty instance vs specific pattern: NO MATCH
        // Pattern requires generate and ext=pdf, instance doesn't have them
        assert!(!empty_urn.conforms_to(&specific_urn).unwrap());

        // Specific instance vs empty pattern: MATCH
        // Pattern has no constraints, instance can have anything
        assert!(specific_urn.conforms_to(&empty_urn).unwrap());

        // Empty instance vs empty pattern: MATCH
        assert!(empty_urn.conforms_to(&empty_urn).unwrap());

        // With trailing semicolon
        let empty_urn2 = TaggedUrn::from_string("cap:;").unwrap();
        assert_eq!(empty_urn2.tags.len(), 0);
    }

    // TEST532: Create empty URN with custom prefix
    #[test]
    fn test_empty_with_custom_prefix() {
        let empty_urn = TaggedUrn::from_string("myapp:").unwrap();
        assert_eq!(empty_urn.get_prefix(), "myapp");
        assert_eq!(empty_urn.tags.len(), 0);
        assert_eq!(empty_urn.to_string(), "myapp:");
    }

    // TEST533: Parse forward slashes and colons in unquoted tag values
    #[test]
    fn test_extended_character_support() {
        // Test forward slashes and colons in tag components
        let urn = TaggedUrn::from_string("cap:url=https://example_org/api;path=/some/file").unwrap();
        assert_eq!(
            urn.get_tag("url"),
            Some(&"https://example_org/api".to_string())
        );
        assert_eq!(urn.get_tag("path"), Some(&"/some/file".to_string()));
    }

    // TEST534: Reject wildcard in keys but accept wildcard in values
    #[test]
    fn test_wildcard_restrictions() {
        // Wildcard should be rejected in keys
        assert!(TaggedUrn::from_string("cap:*=value").is_err());

        // Wildcard should be accepted in values
        let urn = TaggedUrn::from_string("cap:key=*").unwrap();
        assert_eq!(urn.get_tag("key"), Some(&"*".to_string()));
    }

    // TEST535: Reject duplicate keys in URN string
    #[test]
    fn test_duplicate_key_rejection() {
        let result = TaggedUrn::from_string("cap:key=value1;key=value2");
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(matches!(e, TaggedUrnError::DuplicateKey(_)));
        }
    }

    // TEST536: Reject purely numeric keys but allow mixed alphanumeric keys
    #[test]
    fn test_numeric_key_restriction() {
        // Pure numeric keys should be rejected
        assert!(TaggedUrn::from_string("cap:123=value").is_err());

        // Mixed alphanumeric keys should be allowed
        assert!(TaggedUrn::from_string("cap:key123=value").is_ok());
        assert!(TaggedUrn::from_string("cap:123key=value").is_ok());

        // Pure numeric values should be allowed
        assert!(TaggedUrn::from_string("cap:key=123").is_ok());
    }

    // TEST537: Reject empty value after equals sign
    #[test]
    fn test_empty_value_error() {
        assert!(TaggedUrn::from_string("cap:key=").is_err());
        assert!(TaggedUrn::from_string("cap:key=;other=value").is_err());
    }

    // TEST538: Verify has_tag uses case-sensitive value comparison and case-insensitive key lookup
    #[test]
    fn test_has_tag_case_sensitive() {
        let urn = TaggedUrn::from_string(r#"cap:key="Value""#).unwrap();

        // Exact case match works
        assert!(urn.has_tag("key", "Value"));

        // Different case does not match
        assert!(!urn.has_tag("key", "value"));
        assert!(!urn.has_tag("key", "VALUE"));

        // Key lookup is case-insensitive
        assert!(urn.has_tag("KEY", "Value"));
        assert!(urn.has_tag("Key", "Value"));
    }

    // TEST539: Preserve value case when adding tag with with_tag method
    #[test]
    fn test_with_tag_preserves_value() {
        let urn = TaggedUrn::empty("cap".to_string()).with_tag("key".to_string(), "ValueWithCase".to_string()).unwrap();
        assert_eq!(urn.get_tag("key"), Some(&"ValueWithCase".to_string()));
    }

    // TEST540: Reject empty value string in with_tag method
    #[test]
    fn test_with_tag_rejects_empty_value() {
        let result = TaggedUrn::empty("cap".to_string()).with_tag("key".to_string(), "".to_string());
        assert!(result.is_err());
        if let Err(TaggedUrnError::EmptyTagComponent(msg)) = result {
            assert!(msg.contains("empty value"));
        } else {
            panic!("Expected EmptyTagComponent error");
        }
    }

    // TEST541: Reject empty value string in builder tag method
    #[test]
    fn test_builder_rejects_empty_value() {
        let result = TaggedUrnBuilder::new("cap").tag("key", "");
        assert!(result.is_err());
        if let Err(TaggedUrnError::EmptyTagComponent(msg)) = result {
            assert!(msg.contains("empty value"));
        } else {
            panic!("Expected EmptyTagComponent error");
        }
    }

    // TEST542: Treat quoted and unquoted simple lowercase values as semantically equivalent
    #[test]
    fn test_semantic_equivalence() {
        // Unquoted and quoted simple lowercase values are equivalent
        let unquoted = TaggedUrn::from_string("cap:key=simple").unwrap();
        let quoted = TaggedUrn::from_string(r#"cap:key="simple""#).unwrap();
        assert_eq!(unquoted, quoted);

        // Both serialize the same way (unquoted)
        assert_eq!(unquoted.to_string(), "cap:key=simple");
        assert_eq!(quoted.to_string(), "cap:key=simple");
    }

    // ============================================================================
    // MATCHING SEMANTICS SPECIFICATION TESTS
    // These 9 tests verify the exact matching semantics from RULES.md Sections 12-17
    // All implementations (Rust, Go, JS, ObjC) must pass these identically
    // ============================================================================

    // TEST543: Verify exact match when instance and pattern have identical tags
    #[test]
    fn test_matching_semantics_test1_exact_match() {
        // Test 1: Exact match
        // URN:     cap:ext=pdf;generate;in=media:;out=media:
        // Request: cap:ext=pdf;generate;in=media:;out=media:
        // Result:  MATCH
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let request = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        assert!(urn.conforms_to(&request).unwrap(), "Test 1: Exact match should succeed");
    }

    // TEST544: Reject match when instance is missing a tag required by pattern
    #[test]
    fn test_matching_semantics_test2_instance_missing_tag() {
        // Test 2: Instance missing tag
        // Instance: cap:generate;in=media:;out=media:
        // Pattern:  cap:ext=pdf;generate;in=media:;out=media:
        // Result:   NO MATCH (pattern requires ext=pdf, instance doesn't have ext)
        //
        // NEW SEMANTICS: Missing tag in instance means it doesn't exist.
        // Pattern K=v requires instance to have K=v.
        let instance = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();
        let pattern = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        assert!(!instance.conforms_to(&pattern).unwrap(), "Test 2: Instance missing tag should NOT match when pattern requires it");

        // To accept any ext (or missing), use pattern with ext=?
        let pattern_optional = TaggedUrn::from_string("cap:ext=?;generate;in=media:;out=media:").unwrap();
        assert!(instance.conforms_to(&pattern_optional).unwrap(), "Pattern with ext=? should match instance without ext");
    }

    // TEST545: Match when instance has extra tags not constrained by pattern
    #[test]
    fn test_matching_semantics_test3_urn_has_extra_tag() {
        // Test 3: URN has extra tag
        // URN:     cap:ext=pdf;generate;in=media:;out=media:;version=2
        // Request: cap:ext=pdf;generate;in=media:;out=media:
        // Result:  MATCH (request doesn't constrain version)
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;version=2").unwrap();
        let request = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        assert!(urn.conforms_to(&request).unwrap(), "Test 3: URN with extra tag should match");
    }

    // TEST546: Match when pattern has wildcard accepting any value for a tag
    #[test]
    fn test_matching_semantics_test4_request_has_wildcard() {
        // Test 4: Request has wildcard
        // URN:     cap:ext=pdf;generate;in=media:;out=media:
        // Request: cap:ext;generate;in=media:;out=media:
        // Result:  MATCH (request accepts any ext)
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let request = TaggedUrn::from_string("cap:ext;generate;in=media:;out=media:").unwrap();
        assert!(urn.conforms_to(&request).unwrap(), "Test 4: Request wildcard should match");
    }

    // TEST547: Match when instance has wildcard satisfying pattern's specific value
    #[test]
    fn test_matching_semantics_test5_urn_has_wildcard() {
        // Test 5: URN has wildcard
        // URN:     cap:ext;generate;in=media:;out=media:
        // Request: cap:ext=pdf;generate;in=media:;out=media:
        // Result:  MATCH (URN handles any ext)
        let urn = TaggedUrn::from_string("cap:ext;generate;in=media:;out=media:").unwrap();
        let request = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        assert!(urn.conforms_to(&request).unwrap(), "Test 5: URN wildcard should match");
    }

    // TEST548: Reject match when tag values conflict between instance and pattern
    #[test]
    fn test_matching_semantics_test6_value_mismatch() {
        // Test 6: Value mismatch
        // URN:     cap:ext=pdf;generate;in=media:;out=media:
        // Request: cap:ext=docx;generate;in=media:;out=media:
        // Result:  NO MATCH
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let request = TaggedUrn::from_string("cap:ext=docx;generate;in=media:;out=media:").unwrap();
        assert!(!urn.conforms_to(&request).unwrap(), "Test 6: Value mismatch should not match");
    }

    // TEST549: Reject match when pattern requires a tag absent from instance
    #[test]
    fn test_matching_semantics_test7_pattern_has_extra_tag() {
        // Test 7: Pattern has extra tag that instance doesn't have
        // Instance: cap:generate_thumbnail;in=media:;out=media:binary
        // Pattern:  cap:ext=wav;generate_thumbnail;in=media:;out=media:binary
        // Result:   NO MATCH (pattern requires ext=wav, instance doesn't have ext)
        //
        // NEW SEMANTICS: Pattern K=v requires instance to have K=v
        let instance = TaggedUrn::from_string(r#"cap:generate_thumbnail;in=media:;out=media:binary"#).unwrap();
        let pattern = TaggedUrn::from_string(r#"cap:ext=wav;generate_thumbnail;in=media:;out=media:binary"#).unwrap();
        assert!(!instance.conforms_to(&pattern).unwrap(), "Test 7: Instance missing ext should NOT match when pattern requires ext=wav");

        // Instance vs pattern that doesn't constrain ext: MATCH
        let pattern_no_ext = TaggedUrn::from_string(r#"cap:generate_thumbnail;in=media:;out=media:binary"#).unwrap();
        assert!(instance.conforms_to(&pattern_no_ext).unwrap());
    }

    // TEST550: Match any instance against empty pattern with no constraints
    #[test]
    fn test_matching_semantics_test8_empty_pattern_matches_anything() {
        // Test 8: Empty PATTERN matches any INSTANCE
        // Instance: cap:ext=pdf;generate;in=media:;out=media:
        // Pattern:  cap:
        // Result:   MATCH (pattern has no constraints)
        //
        // NEW SEMANTICS: Empty pattern = no constraints = matches any instance
        // But empty instance only matches patterns that don't require tags
        let instance = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let empty_pattern = TaggedUrn::from_string("cap:").unwrap();
        assert!(instance.conforms_to(&empty_pattern).unwrap(), "Test 8: Any instance should match empty pattern");

        // Empty instance vs pattern with requirements: NO MATCH
        let empty_instance = TaggedUrn::from_string("cap:").unwrap();
        let pattern = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        assert!(!empty_instance.conforms_to(&pattern).unwrap(), "Empty instance should NOT match pattern with requirements");
    }

    // TEST551: Reject match when instance and pattern have non-overlapping tag dimensions
    #[test]
    fn test_matching_semantics_test9_cross_dimension_constraints() {
        // Test 9: Cross-dimension constraints
        // Instance: cap:generate;in=media:;out=media:
        // Pattern:  cap:ext=pdf
        // Result:   NO MATCH (pattern requires ext=pdf, instance doesn't have ext)
        //
        // NEW SEMANTICS: Pattern K=v requires instance to have K=v
        let instance = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();
        let pattern = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        assert!(!instance.conforms_to(&pattern).unwrap(), "Test 9: Instance without ext should NOT match pattern requiring ext");

        // Instance with ext vs pattern with different tag only: MATCH
        let instance2 = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let pattern2 = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        assert!(instance2.conforms_to(&pattern2).unwrap(), "Instance with ext=pdf should match pattern requiring ext=pdf");
    }

    // TEST552: Return error for conforms_to, accepts, and is_more_specific_than with different prefixes
    #[test]
    fn test_matching_different_prefixes_error() {
        // URNs with different prefixes should cause an error, not just return false
        let urn1 = TaggedUrn::from_string("cap:in=media:;out=media:;test").unwrap();
        let urn2 = TaggedUrn::from_string("other:test").unwrap();

        let result = urn1.conforms_to(&urn2);
        assert!(result.is_err());

        let result2 = urn1.accepts(&urn2);
        assert!(result2.is_err());

        let result3 = urn1.is_more_specific_than(&urn2);
        assert!(result3.is_err());
    }

    // ============================================================================
    // VALUE-LESS TAG TESTS
    // Value-less tags are equivalent to wildcard tags (key=*)
    // ============================================================================

    // TEST553: Parse single value-less tag as wildcard
    #[test]
    fn test_valueless_tag_parsing_single() {
        // Single value-less tag
        let urn = TaggedUrn::from_string("cap:optimize").unwrap();
        assert_eq!(urn.get_tag("optimize"), Some(&"*".to_string()));
        // Serializes as value-less (no =*)
        assert_eq!(urn.to_string(), "cap:optimize");
    }

    // TEST554: Parse multiple value-less tags and serialize alphabetically
    #[test]
    fn test_valueless_tag_parsing_multiple() {
        // Multiple value-less tags
        let urn = TaggedUrn::from_string("cap:fast;optimize;secure").unwrap();
        assert_eq!(urn.get_tag("fast"), Some(&"*".to_string()));
        assert_eq!(urn.get_tag("optimize"), Some(&"*".to_string()));
        assert_eq!(urn.get_tag("secure"), Some(&"*".to_string()));
        // Serializes alphabetically as value-less
        assert_eq!(urn.to_string(), "cap:fast;optimize;secure");
    }

    // TEST555: Parse mix of value-less and valued tags together
    #[test]
    fn test_valueless_tag_mixed_with_valued() {
        // Mix of value-less and valued tags
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;optimize;out=media:;secure").unwrap();
        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.get_tag("optimize"), Some(&"*".to_string()));
        assert_eq!(urn.get_tag("ext"), Some(&"pdf".to_string()));
        assert_eq!(urn.get_tag("secure"), Some(&"*".to_string()));
        // Serializes alphabetically
        assert_eq!(urn.to_string(), "cap:ext=pdf;generate;in=media:;optimize;out=media:;secure");
    }

    // TEST556: Parse value-less tag at end of URN without trailing semicolon
    #[test]
    fn test_valueless_tag_at_end() {
        // Value-less tag at the end (no trailing semicolon)
        let urn = TaggedUrn::from_string("cap:generate;in=media:;optimize;out=media:").unwrap();
        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.get_tag("optimize"), Some(&"*".to_string()));
        assert_eq!(urn.to_string(), "cap:generate;in=media:;optimize;out=media:");
    }

    // TEST557: Verify value-less tag is equivalent to explicit wildcard (key=*)
    #[test]
    fn test_valueless_tag_equivalence_to_wildcard() {
        // Value-less tag is equivalent to explicit wildcard
        let valueless = TaggedUrn::from_string("cap:ext").unwrap();
        let wildcard = TaggedUrn::from_string("cap:ext=*").unwrap();
        assert_eq!(valueless, wildcard);
        // Both serialize to value-less form
        assert_eq!(valueless.to_string(), "cap:ext");
        assert_eq!(wildcard.to_string(), "cap:ext");
    }

    // TEST558: Match value-less wildcard tag against any specific value
    #[test]
    fn test_valueless_tag_matching() {
        // Value-less tag (wildcard) matches any value
        let urn = TaggedUrn::from_string("cap:ext;generate;in=media:;out=media:").unwrap();

        let request_pdf = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let request_docx = TaggedUrn::from_string("cap:ext=docx;generate;in=media:;out=media:").unwrap();
        let request_any = TaggedUrn::from_string("cap:ext=anything;generate;in=media:;out=media:").unwrap();

        assert!(urn.conforms_to(&request_pdf).unwrap());
        assert!(urn.conforms_to(&request_docx).unwrap());
        assert!(urn.conforms_to(&request_any).unwrap());
    }

    // TEST559: Require value-less tag in pattern to be present in instance
    #[test]
    fn test_valueless_tag_in_pattern() {
        // Pattern with value-less tag (K=*) requires instance to have the tag
        let pattern = TaggedUrn::from_string("cap:ext;generate;in=media:;out=media:").unwrap();

        let instance_pdf = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let instance_docx = TaggedUrn::from_string("cap:ext=docx;generate;in=media:;out=media:").unwrap();
        let instance_missing = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();

        // NEW SEMANTICS: K=* (valueless tag) means must-have-any
        assert!(instance_pdf.conforms_to(&pattern).unwrap()); // Has ext=pdf
        assert!(instance_docx.conforms_to(&pattern).unwrap()); // Has ext=docx
        assert!(!instance_missing.conforms_to(&pattern).unwrap()); // Missing ext, pattern requires it

        // To accept missing ext, use ? instead
        let pattern_optional = TaggedUrn::from_string("cap:ext=?;generate;in=media:;out=media:").unwrap();
        assert!(instance_missing.conforms_to(&pattern_optional).unwrap());
    }

    // TEST560: Score value-less wildcard tags with graded specificity
    #[test]
    fn test_valueless_tag_specificity() {
        // Six-form ladder: ?x=0, x?=v=1, x=*=2, x!=v=3, x=v=4, !x=5.
        let urn1 = TaggedUrn::from_string("cap:generate").unwrap();          // 1 marker
        let urn2 = TaggedUrn::from_string("cap:generate;optimize").unwrap(); // 2 markers
        let urn3 = TaggedUrn::from_string("cap:ext=pdf;generate").unwrap();  // 1 marker + 1 exact

        assert_eq!(urn1.specificity(), 2); // 1 marker = 2
        assert_eq!(urn2.specificity(), 4); // 2 markers = 2 + 2 = 4
        assert_eq!(urn3.specificity(), 6); // 1 marker + 1 exact = 2 + 4 = 6
    }

    // TEST561: Round-trip value-less tags through parse and serialize
    #[test]
    fn test_valueless_tag_roundtrip() {
        // Round-trip parsing and serialization
        let original = "cap:ext=pdf;generate;in=media:;optimize;out=media:;secure";
        let urn = TaggedUrn::from_string(original).unwrap();
        let serialized = urn.to_string();
        let reparsed = TaggedUrn::from_string(&serialized).unwrap();
        assert_eq!(urn, reparsed);
        assert_eq!(serialized, original);
    }

    // TEST562: Normalize value-less tag keys to lowercase
    #[test]
    fn test_valueless_tag_case_normalization() {
        // Value-less tags are normalized to lowercase like other keys
        let urn = TaggedUrn::from_string("cap:OPTIMIZE;Fast;SECURE").unwrap();
        assert_eq!(urn.get_tag("optimize"), Some(&"*".to_string()));
        assert_eq!(urn.get_tag("fast"), Some(&"*".to_string()));
        assert_eq!(urn.get_tag("secure"), Some(&"*".to_string()));
        assert_eq!(urn.to_string(), "cap:fast;optimize;secure");
    }

    // TEST563: Reject empty value with equals sign as distinct from value-less tag
    #[test]
    fn test_empty_value_still_error() {
        // Empty value with = is still an error (different from value-less)
        assert!(TaggedUrn::from_string("cap:key=").is_err());
        assert!(TaggedUrn::from_string("cap:key=;other=value").is_err());
    }

    // TEST564: Verify directional accepts of value-less wildcard tags with specific values
    #[test]
    fn test_valueless_tag_directional_accepts() {
        // Value-less tags stored as * act as pattern requiring any present value
        let wildcard_ext = TaggedUrn::from_string("cap:ext;generate;in=media:;out=media:").unwrap();
        let ext_pdf = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let ext_docx = TaggedUrn::from_string("cap:ext=docx;generate;in=media:;out=media:").unwrap();

        // wildcard ext=* accepts specific ext=pdf (pattern * accepts any value)
        assert!(wildcard_ext.accepts(&ext_pdf).unwrap());
        // specific ext=pdf conforms_to wildcard ext=* (instance value satisfies * pattern)
        assert!(ext_pdf.conforms_to(&wildcard_ext).unwrap());
        // Same for docx
        assert!(wildcard_ext.accepts(&ext_docx).unwrap());
        // But pdf and docx don't accept each other (different exact values)
        assert!(!ext_pdf.accepts(&ext_docx).unwrap());
        assert!(!ext_docx.accepts(&ext_pdf).unwrap());
    }

    // TEST565: Reject purely numeric keys for value-less tags
    #[test]
    fn test_valueless_numeric_key_still_rejected() {
        // Purely numeric keys are still rejected for value-less tags
        assert!(TaggedUrn::from_string("cap:123").is_err());
        assert!(TaggedUrn::from_string("cap:generate;in=media:;out=media:;456").is_err());
    }

    // TEST566: Reject leading, trailing, and embedded whitespace in URN input
    #[test]
    fn test_whitespace_in_input_rejected() {
        // Leading whitespace fails hard
        let result = TaggedUrn::from_string(" cap:in=media:;out=media:;test");
        assert!(result.is_err());
        if let Err(TaggedUrnError::WhitespaceInInput(_)) = result {
            // Expected
        } else {
            panic!("Expected WhitespaceInInput error, got {:?}", result);
        }

        // Trailing whitespace fails hard
        let result = TaggedUrn::from_string("cap:in=media:;out=media:;test ");
        assert!(result.is_err());
        if let Err(TaggedUrnError::WhitespaceInInput(_)) = result {
            // Expected
        } else {
            panic!("Expected WhitespaceInInput error, got {:?}", result);
        }

        // Both leading and trailing whitespace fails hard
        let result = TaggedUrn::from_string(" cap:in=media:;out=media:;test ");
        assert!(result.is_err());
        if let Err(TaggedUrnError::WhitespaceInInput(_)) = result {
            // Expected
        } else {
            panic!("Expected WhitespaceInInput error, got {:?}", result);
        }

        // Tab and newline also count as whitespace
        assert!(TaggedUrn::from_string("\tcap:in=media:;out=media:;test").is_err());
        assert!(TaggedUrn::from_string("cap:in=media:;out=media:;test\n").is_err());

        // Clean input works
        assert!(TaggedUrn::from_string("cap:in=media:;out=media:;test").is_ok());
    }

    // ============================================================================
    // NEW SEMANTICS TESTS: ? (unspecified) and ! (must-not-have)
    // ============================================================================

    // TEST567: Parse question mark as unspecified value and verify
    // serialization. All three input aliases (?x, x?, x=?) parse to
    // the same stored value `"?"` and serialize as the canonical
    // prefix form `?x`.
    #[test]
    fn test_unspecified_question_mark_parsing() {
        let urn = TaggedUrn::from_string("cap:ext=?").unwrap();
        assert_eq!(urn.get_tag("ext"), Some(&"?".to_string()));
        // Canonical form is `?ext` (prefix), not `ext=?`.
        assert_eq!(urn.to_string(), "cap:?ext");
    }

    // TEST568: Parse exclamation mark as must-not-have value and
    // verify serialization. All three input aliases (!x, x!, x=!)
    // parse to stored value `"!"` and serialize as canonical `!x`.
    #[test]
    fn test_must_not_have_exclamation_parsing() {
        let urn = TaggedUrn::from_string("cap:ext=!").unwrap();
        assert_eq!(urn.get_tag("ext"), Some(&"!".to_string()));
        // Canonical form is `!ext` (prefix), not `ext=!`.
        assert_eq!(urn.to_string(), "cap:!ext");
    }

    // TEST569: Match any instance against pattern with unspecified (?) tag value
    #[test]
    fn test_question_mark_pattern_matches_anything() {
        // Pattern with K=? matches any instance (with or without K)
        let pattern = TaggedUrn::from_string("cap:ext=?").unwrap();

        let instance_pdf = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        let instance_docx = TaggedUrn::from_string("cap:ext=docx").unwrap();
        let instance_missing = TaggedUrn::from_string("cap:").unwrap();
        let instance_wildcard = TaggedUrn::from_string("cap:ext=*").unwrap();
        let instance_must_not = TaggedUrn::from_string("cap:ext=!").unwrap();

        assert!(instance_pdf.conforms_to(&pattern).unwrap(), "ext=pdf should match ext=?");
        assert!(instance_docx.conforms_to(&pattern).unwrap(), "ext=docx should match ext=?");
        assert!(instance_missing.conforms_to(&pattern).unwrap(), "(no ext) should match ext=?");
        assert!(instance_wildcard.conforms_to(&pattern).unwrap(), "ext=* should match ext=?");
        assert!(instance_must_not.conforms_to(&pattern).unwrap(), "ext=! should match ext=?");
    }

    // TEST570: Match instance with unspecified (?) tag against any pattern constraint
    #[test]
    fn test_question_mark_in_instance() {
        // Instance with K=? matches any pattern constraint
        let instance = TaggedUrn::from_string("cap:ext=?").unwrap();

        let pattern_pdf = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        let pattern_wildcard = TaggedUrn::from_string("cap:ext=*").unwrap();
        let pattern_must_not = TaggedUrn::from_string("cap:ext=!").unwrap();
        let pattern_question = TaggedUrn::from_string("cap:ext=?").unwrap();
        let pattern_missing = TaggedUrn::from_string("cap:").unwrap();

        assert!(instance.conforms_to(&pattern_pdf).unwrap(), "ext=? should match ext=pdf");
        assert!(instance.conforms_to(&pattern_wildcard).unwrap(), "ext=? should match ext=*");
        assert!(instance.conforms_to(&pattern_must_not).unwrap(), "ext=? should match ext=!");
        assert!(instance.conforms_to(&pattern_question).unwrap(), "ext=? should match ext=?");
        assert!(instance.conforms_to(&pattern_missing).unwrap(), "ext=? should match (no ext)");
    }

    // TEST571: Require tag to be absent when pattern uses must-not-have (!) value
    #[test]
    fn test_must_not_have_pattern_requires_absent() {
        // Pattern with K=! requires instance to NOT have K
        let pattern = TaggedUrn::from_string("cap:ext=!").unwrap();

        let instance_missing = TaggedUrn::from_string("cap:").unwrap();
        let instance_pdf = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        let instance_wildcard = TaggedUrn::from_string("cap:ext=*").unwrap();
        let instance_must_not = TaggedUrn::from_string("cap:ext=!").unwrap();

        assert!(instance_missing.conforms_to(&pattern).unwrap(), "(no ext) should match ext=!");
        assert!(!instance_pdf.conforms_to(&pattern).unwrap(), "ext=pdf should NOT match ext=!");
        assert!(!instance_wildcard.conforms_to(&pattern).unwrap(), "ext=* should NOT match ext=!");
        assert!(instance_must_not.conforms_to(&pattern).unwrap(), "ext=! should match ext=!");
    }

    // TEST572: Reject instance with must-not-have (!) tag against patterns requiring that tag
    #[test]
    fn test_must_not_have_in_instance() {
        // Instance with K=! conflicts with patterns requiring K
        let instance = TaggedUrn::from_string("cap:ext=!").unwrap();

        let pattern_pdf = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        let pattern_wildcard = TaggedUrn::from_string("cap:ext=*").unwrap();
        let pattern_must_not = TaggedUrn::from_string("cap:ext=!").unwrap();
        let pattern_question = TaggedUrn::from_string("cap:ext=?").unwrap();
        let pattern_missing = TaggedUrn::from_string("cap:").unwrap();

        assert!(!instance.conforms_to(&pattern_pdf).unwrap(), "ext=! should NOT match ext=pdf");
        assert!(!instance.conforms_to(&pattern_wildcard).unwrap(), "ext=! should NOT match ext=*");
        assert!(instance.conforms_to(&pattern_must_not).unwrap(), "ext=! should match ext=!");
        assert!(instance.conforms_to(&pattern_question).unwrap(), "ext=! should match ext=?");
        assert!(instance.conforms_to(&pattern_missing).unwrap(), "ext=! should match (no ext)");
    }

    // TEST573: Verify full cross-product truth table for all instance/pattern value combinations
    #[test]
    fn test_full_cross_product_matching() {
        // Comprehensive test of all instance/pattern combinations
        // Based on the truth table in the plan

        // Helper to test a single case
        fn check(instance: &str, pattern: &str, expected: bool, msg: &str) {
            let inst = TaggedUrn::from_string(instance).unwrap();
            let patt = TaggedUrn::from_string(pattern).unwrap();
            assert_eq!(
                inst.conforms_to(&patt).unwrap(),
                expected,
                "{}: instance={}, pattern={}",
                msg,
                instance,
                pattern
            );
        }

        // Instance missing, Pattern variations
        check("cap:", "cap:", true, "(none)/(none)");
        check("cap:", "cap:k=?", true, "(none)/K=?");
        check("cap:", "cap:k=!", true, "(none)/K=!");
        check("cap:", "cap:k", false, "(none)/K=*");  // K is valueless = *
        check("cap:", "cap:k=v", false, "(none)/K=v");

        // Instance K=?, Pattern variations
        check("cap:k=?", "cap:", true, "K=?/(none)");
        check("cap:k=?", "cap:k=?", true, "K=?/K=?");
        check("cap:k=?", "cap:k=!", true, "K=?/K=!");
        check("cap:k=?", "cap:k", true, "K=?/K=*");
        check("cap:k=?", "cap:k=v", true, "K=?/K=v");

        // Instance K=!, Pattern variations
        check("cap:k=!", "cap:", true, "K=!/(none)");
        check("cap:k=!", "cap:k=?", true, "K=!/K=?");
        check("cap:k=!", "cap:k=!", true, "K=!/K=!");
        check("cap:k=!", "cap:k", false, "K=!/K=*");
        check("cap:k=!", "cap:k=v", false, "K=!/K=v");

        // Instance K=*, Pattern variations
        check("cap:k", "cap:", true, "K=*/(none)");
        check("cap:k", "cap:k=?", true, "K=*/K=?");
        check("cap:k", "cap:k=!", false, "K=*/K=!");
        check("cap:k", "cap:k", true, "K=*/K=*");
        check("cap:k", "cap:k=v", true, "K=*/K=v");

        // Instance K=v, Pattern variations
        check("cap:k=v", "cap:", true, "K=v/(none)");
        check("cap:k=v", "cap:k=?", true, "K=v/K=?");
        check("cap:k=v", "cap:k=!", false, "K=v/K=!");
        check("cap:k=v", "cap:k", true, "K=v/K=*");
        check("cap:k=v", "cap:k=v", true, "K=v/K=v");
        check("cap:k=v", "cap:k=w", false, "K=v/K=w");
    }

    // TEST574: Match URN with mixed required, optional, forbidden, and exact tags
    #[test]
    fn test_mixed_special_values() {
        // Test URNs with multiple special values
        let pattern = TaggedUrn::from_string("cap:required;optional=?;forbidden=!;exact=pdf").unwrap();

        // Instance that satisfies all constraints
        let good_instance = TaggedUrn::from_string("cap:required=yes;optional=maybe;exact=pdf").unwrap();
        assert!(good_instance.conforms_to(&pattern).unwrap());

        // Instance missing required tag
        let missing_required = TaggedUrn::from_string("cap:optional=maybe;exact=pdf").unwrap();
        assert!(!missing_required.conforms_to(&pattern).unwrap());

        // Instance has forbidden tag
        let has_forbidden = TaggedUrn::from_string("cap:required=yes;forbidden=oops;exact=pdf").unwrap();
        assert!(!has_forbidden.conforms_to(&pattern).unwrap());

        // Instance with wrong exact value
        let wrong_exact = TaggedUrn::from_string("cap:required=yes;exact=doc").unwrap();
        assert!(!wrong_exact.conforms_to(&pattern).unwrap());
    }

    // TEST575: Round-trip all special values (?, !, *, exact) through parse and serialize
    #[test]
    fn test_serialization_round_trip_special_values() {
        // All special values round-trip correctly
        let originals = [
            "cap:ext=?",
            "cap:ext=!",
            "cap:ext",  // * serializes as valueless
            "cap:a=?;b=!;c;d=exact",
        ];

        for original in originals {
            let urn = TaggedUrn::from_string(original).unwrap();
            let serialized = urn.to_string();
            let reparsed = TaggedUrn::from_string(&serialized).unwrap();
            assert_eq!(urn, reparsed, "Round-trip failed for: {}", original);
        }
    }

    // TEST576: Check bidirectional accepts between !, *, ?, and specific value tags
    #[test]
    fn test_bidirectional_accepts_with_special_values() {
        // ! does not overlap with * or specific values
        let must_not = TaggedUrn::from_string("cap:ext=!").unwrap();
        let must_have = TaggedUrn::from_string("cap:ext=*").unwrap();
        let specific = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        let unspecified = TaggedUrn::from_string("cap:ext=?").unwrap();
        let missing = TaggedUrn::from_string("cap:").unwrap();

        assert!(!(must_not.accepts(&must_have).unwrap() || must_have.accepts(&must_not).unwrap()));
        assert!(!(must_not.accepts(&specific).unwrap() || specific.accepts(&must_not).unwrap()));
        assert!(must_not.accepts(&unspecified).unwrap() || unspecified.accepts(&must_not).unwrap());
        assert!(must_not.accepts(&missing).unwrap() || missing.accepts(&must_not).unwrap());
        assert!(must_not.accepts(&must_not).unwrap() || must_not.accepts(&must_not).unwrap());

        // * overlaps with specific values
        assert!(must_have.accepts(&specific).unwrap() || specific.accepts(&must_have).unwrap());
        assert!(must_have.accepts(&must_have).unwrap() || must_have.accepts(&must_have).unwrap());

        // ? overlaps with everything
        assert!(unspecified.accepts(&must_not).unwrap() || must_not.accepts(&unspecified).unwrap());
        assert!(unspecified.accepts(&must_have).unwrap() || must_have.accepts(&unspecified).unwrap());
        assert!(unspecified.accepts(&specific).unwrap() || specific.accepts(&unspecified).unwrap());
        assert!(unspecified.accepts(&unspecified).unwrap() || unspecified.accepts(&unspecified).unwrap());
        assert!(unspecified.accepts(&missing).unwrap() || missing.accepts(&unspecified).unwrap());
    }

    // =========================================================================
    // ORDER-THEORETIC RELATIONS: is_equivalent, is_comparable
    // =========================================================================

    // TEST578: Equivalent URNs with identical tag sets
    #[test]
    fn test578_equivalent_identical_tags() {
        let a = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let b = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap(); // same tags, different order
        assert!(a.is_equivalent(&b).unwrap());
        assert!(b.is_equivalent(&a).unwrap()); // symmetric
    }

    // TEST579: Non-equivalent URNs where one is more specific
    #[test]
    fn test579_not_equivalent_when_one_more_specific() {
        let general = TaggedUrn::from_string("media:").unwrap();
        let specific = TaggedUrn::from_string("media:pdf").unwrap();
        assert!(!general.is_equivalent(&specific).unwrap());
        assert!(!specific.is_equivalent(&general).unwrap());
    }

    // TEST580: Comparable URNs on the same specialization chain
    #[test]
    fn test580_comparable_specialization_chain() {
        let general = TaggedUrn::from_string("media:").unwrap();
        let specific = TaggedUrn::from_string("media:pdf").unwrap();
        // general.accepts(specific) = true (wildcard ⊆ pdf)
        // specific.accepts(general) = false (pdf missing from general)
        // OR → true
        assert!(general.is_comparable(&specific).unwrap());
        assert!(specific.is_comparable(&general).unwrap()); // symmetric
    }

    // TEST581: Incomparable URNs in different branches of the lattice
    #[test]
    fn test581_incomparable_different_branches() {
        let pdf = TaggedUrn::from_string("media:pdf").unwrap();
        let txt = TaggedUrn::from_string("media:txt;textable").unwrap();
        // pdf.accepts(txt) = false (pdf missing from txt)
        // txt.accepts(pdf) = false (txt missing from pdf)
        // OR → false
        assert!(!pdf.is_comparable(&txt).unwrap());
        assert!(!txt.is_comparable(&pdf).unwrap());
    }

    // TEST582: Equivalent implies comparable but not vice versa
    #[test]
    fn test582_equivalent_implies_comparable() {
        let a = TaggedUrn::from_string("cap:ext=pdf;in=media:;out=media:;test").unwrap();
        let b = TaggedUrn::from_string("cap:ext=pdf;in=media:;out=media:;test").unwrap();
        // equivalent → comparable (AND implies OR)
        assert!(a.is_equivalent(&b).unwrap());
        assert!(a.is_comparable(&b).unwrap());

        // comparable but NOT equivalent
        let general = TaggedUrn::from_string("cap:in=media:;out=media:;test").unwrap();
        let specific = TaggedUrn::from_string("cap:ext=pdf;in=media:;out=media:;test").unwrap();
        assert!(!general.is_equivalent(&specific).unwrap());
        assert!(general.is_comparable(&specific).unwrap());
    }

    // TEST583: Prefix mismatch returns error for both relations
    #[test]
    fn test583_prefix_mismatch_errors() {
        let cap = TaggedUrn::from_string("cap:in=media:;out=media:;test").unwrap();
        let media = TaggedUrn::from_string("media:").unwrap();
        assert!(cap.is_equivalent(&media).is_err());
        assert!(cap.is_comparable(&media).is_err());
    }

    // TEST584: Empty tag set is comparable to everything with same prefix
    #[test]
    fn test584_empty_tags_comparable_to_all() {
        let empty = TaggedUrn::from_string("media:").unwrap();
        let specific = TaggedUrn::from_string("media:pdf;thumbnail").unwrap();
        // empty.accepts(specific) = true (empty has no constraints)
        assert!(empty.is_comparable(&specific).unwrap());
        // but NOT equivalent (specific has tags empty doesn't)
        assert!(!empty.is_equivalent(&specific).unwrap());
        // empty is equivalent to itself
        let empty2 = TaggedUrn::from_string("media:").unwrap();
        assert!(empty.is_equivalent(&empty2).unwrap());
    }

    // TEST585: String variants of is_equivalent and is_comparable
    #[test]
    fn test585_string_variants() {
        let urn = TaggedUrn::from_string("media:pdf").unwrap();
        assert!(urn.is_equivalent_str("media:pdf").unwrap()); // same tags
        assert!(!urn.is_equivalent_str("media:").unwrap()); // different
        assert!(urn.is_comparable_str("media:").unwrap()); // on same chain
        assert!(!urn.is_comparable_str("media:txt;textable").unwrap()); // different branch
    }

    // TEST586: Special values (*, !, ?) with is_equivalent and is_comparable
    #[test]
    fn test586_special_values() {
        let must_have = TaggedUrn::from_string("cap:ext").unwrap(); // ext=*
        let exact = TaggedUrn::from_string("cap:ext=pdf").unwrap(); // ext=pdf
        let must_not = TaggedUrn::from_string("cap:ext=!").unwrap(); // ext=!
        let unspecified = TaggedUrn::from_string("cap:ext=?").unwrap(); // ext=?

        // must_have (*) and exact (pdf): equivalent — * accepts any value
        // bidirectionally (instance * is fine with pattern pdf, pattern * accepts instance pdf)
        assert!(must_have.is_equivalent(&exact).unwrap());
        assert!(must_have.is_comparable(&exact).unwrap());

        // must_not (!) and exact (pdf): incomparable (conflict both directions)
        assert!(!must_not.is_comparable(&exact).unwrap());
        assert!(!must_not.is_equivalent(&exact).unwrap());

        // must_not (!) and must_have (*): incomparable (conflict both directions)
        assert!(!must_not.is_comparable(&must_have).unwrap());
        assert!(!must_not.is_equivalent(&must_have).unwrap());

        // unspecified (?) is equivalent to everything — ? matches anything
        assert!(unspecified.is_equivalent(&exact).unwrap());
        assert!(unspecified.is_equivalent(&must_have).unwrap());
        assert!(unspecified.is_equivalent(&must_not).unwrap());
    }

    // TEST577: Verify graded specificity scores and tuples for special value types
    // under the six-form ladder.
    #[test]
    fn test_specificity_with_special_values() {
        // Six-form ladder: ?x=0, x?=v=1, x=*=2, x!=v=3, x=v=4, !x=5
        let exact = TaggedUrn::from_string("cap:a=x;b=y;c=z").unwrap();      // 3 * 4 = 12
        let must_have = TaggedUrn::from_string("cap:a;b;c").unwrap();        // 3 * 2 = 6
        let must_not = TaggedUrn::from_string("cap:!a;!b;!c").unwrap();      // 3 * 5 = 15
        let unspecified = TaggedUrn::from_string("cap:?a;?b;?c").unwrap();   // 3 * 0 = 0
        // mixed: a=x (4) + b (2) + !c (5) + ?d (0) = 11
        let mixed = TaggedUrn::from_string("cap:!c;?d;a=x;b").unwrap();

        assert_eq!(exact.specificity(), 12);
        assert_eq!(must_have.specificity(), 6);
        assert_eq!(must_not.specificity(), 15);
        assert_eq!(unspecified.specificity(), 0);
        assert_eq!(mixed.specificity(), 11);

        // Five-tuple form-bucket counts:
        //   (must_not_have, exact, present_not_value, must_have_any, absent_or_not_value)
        assert_eq!(exact.specificity_tuple(), (0, 3, 0, 0, 0));
        assert_eq!(must_have.specificity_tuple(), (0, 0, 0, 3, 0));
        assert_eq!(must_not.specificity_tuple(), (3, 0, 0, 0, 0));
        assert_eq!(unspecified.specificity_tuple(), (0, 0, 0, 0, 0));
        assert_eq!(mixed.specificity_tuple(), (1, 1, 0, 1, 0));
    }

    // =========================================================================
    // BUILDER TESTS (mirroring ObjC CSTaggedUrnBuilderTests)
    // =========================================================================

    // TEST587: Builder fluent API for tag manipulation
    #[test]
    fn test587_builder_fluent_api() {
        let urn = TaggedUrnBuilder::new("cap")
            .marker("generate")
            .tag("target", "thumbnail").unwrap()
            .tag("format", "pdf").unwrap()
            .tag("output", "binary").unwrap()
            .build()
            .unwrap();

        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.get_tag("target"), Some(&"thumbnail".to_string()));
        assert_eq!(urn.get_tag("format"), Some(&"pdf".to_string()));
        assert_eq!(urn.get_tag("output"), Some(&"binary".to_string()));
    }

    // TEST588: Builder with custom tags
    #[test]
    fn test588_builder_custom_tags() {
        let urn = TaggedUrnBuilder::new("cap")
            .tag("engine", "v2").unwrap()
            .tag("quality", "high").unwrap()
            .marker("compress")
            .build()
            .unwrap();

        assert_eq!(urn.get_tag("engine"), Some(&"v2".to_string()));
        assert_eq!(urn.get_tag("quality"), Some(&"high".to_string()));
        assert!(urn.has_marker_tag("compress"));
    }

    // TEST589: Builder tag overrides (last value wins)
    #[test]
    fn test589_builder_tag_overrides() {
        let urn = TaggedUrnBuilder::new("cap")
            .marker("convert")
            .tag("format", "jpg").unwrap()
            .build()
            .unwrap();

        assert!(urn.has_marker_tag("convert"));
        assert_eq!(urn.get_tag("format"), Some(&"jpg".to_string()));
    }

    // TEST590: Builder empty build returns error (tags required)
    #[test]
    fn test590_builder_empty_build() {
        // Empty builder returns error - tags are required
        let result = TaggedUrnBuilder::new("cap").build();
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(matches!(e, TaggedUrnError::Empty));
        }
    }

    // TEST591: Builder with single tag
    #[test]
    fn test591_builder_single_tag() {
        let urn = TaggedUrnBuilder::new("cap")
            .tag("type", "utility").unwrap()
            .build()
            .unwrap();

        assert_eq!(urn.to_string(), "cap:type=utility");
        assert_eq!(urn.get_tag("type"), Some(&"utility".to_string()));
        // Six-form ladder: exact value = 4 points.
        assert_eq!(urn.specificity(), 4);
    }

    // TEST592: Builder with complex multi-tag URN
    #[test]
    fn test592_builder_complex() {
        let urn = TaggedUrnBuilder::new("cap")
            .tag("type", "media").unwrap()
            .marker("transcode")
            .tag("target", "video").unwrap()
            .tag("format", "mp4").unwrap()
            .tag("codec", "h264").unwrap()
            .tag("quality", "1080p").unwrap()
            .tag("framerate", "30fps").unwrap()
            .tag("output", "binary").unwrap()
            .build()
            .unwrap();

        assert_eq!(urn.get_tag("type"), Some(&"media".to_string()));
        assert!(urn.has_marker_tag("transcode"));
        assert_eq!(urn.get_tag("target"), Some(&"video".to_string()));
        assert_eq!(urn.get_tag("format"), Some(&"mp4".to_string()));
        assert_eq!(urn.get_tag("codec"), Some(&"h264".to_string()));
        assert_eq!(urn.get_tag("quality"), Some(&"1080p".to_string()));
        assert_eq!(urn.get_tag("framerate"), Some(&"30fps".to_string()));
        assert_eq!(urn.get_tag("output"), Some(&"binary".to_string()));

        // Six-form ladder: 7 exact-valued tags × 4 + 1 marker (transcode) × 2 = 28 + 2 = 30.
        assert_eq!(urn.specificity(), 30);
    }

    // TEST593: Builder with wildcards
    #[test]
    fn test593_builder_wildcards() {
        let urn = TaggedUrnBuilder::new("cap")
            .marker("convert")
            .marker("ext")
            .marker("quality")
            .build()
            .unwrap();

        // All three markers serialize as value-less, sorted alphabetically.
        assert_eq!(urn.to_string(), "cap:convert;ext;quality");
        // GRADED SPECIFICITY: 3 markers × 2 points each = 6
        assert_eq!(urn.specificity(), 6);

        assert!(urn.has_marker_tag("convert"));
        assert!(urn.has_marker_tag("ext"));
        assert!(urn.has_marker_tag("quality"));
    }

    // TEST594: Builder with custom prefix
    #[test]
    fn test594_builder_custom_prefix() {
        let urn = TaggedUrnBuilder::new("myapp")
            .tag("key", "value").unwrap()
            .build()
            .unwrap();

        assert_eq!(urn.prefix, "myapp");
        assert_eq!(urn.to_string(), "myapp:key=value");
    }

    // TEST595: Builder matching with built URN
    #[test]
    fn test595_builder_matching_with_built_urn() {
        // Create a specific instance
        let specific_instance = TaggedUrnBuilder::new("cap")
            .tag("op", "generate").unwrap()
            .tag("target", "thumbnail").unwrap()
            .tag("format", "pdf").unwrap()
            .build()
            .unwrap();

        // Create a more general pattern (fewer constraints)
        let general_pattern = TaggedUrnBuilder::new("cap")
            .tag("op", "generate").unwrap()
            .build()
            .unwrap();

        // Create a pattern with wildcard (ext=* means must-have-any)
        let wildcard_pattern = TaggedUrnBuilder::new("cap")
            .tag("op", "generate").unwrap()
            .tag("target", "thumbnail").unwrap()
            .tag("ext", "*").unwrap()
            .build()
            .unwrap();

        // Specific instance should match general pattern (pattern has fewer constraints)
        assert!(specific_instance.conforms_to(&general_pattern).unwrap());

        // NEW SEMANTICS: wildcardPattern has ext=* which means instance MUST have ext
        // specificInstance doesn't have ext, so this should NOT match
        assert!(!specific_instance.conforms_to(&wildcard_pattern).unwrap());

        // Check specificity
        assert!(specific_instance.is_more_specific_than(&general_pattern).unwrap());

        // Six-form ladder: exact = 4 points, * (must-have-any) = 2 points.
        assert_eq!(specific_instance.specificity(), 12); // 3 exact × 4 = 12
        assert_eq!(general_pattern.specificity(), 4);    // 1 exact × 4 = 4
        assert_eq!(wildcard_pattern.specificity(), 10);  // 2 exact × 4 + 1 * × 2 = 8 + 2 = 10
    }
}

#[test]
fn test_tag_order_normalization() {
    // Two URNs with same tags in different order should produce identical canonical string
    let urn1 = TaggedUrn::from_string("media:list;textable").unwrap();
    let urn2 = TaggedUrn::from_string("media:textable;list").unwrap();
    
    eprintln!("urn1: {}", urn1.to_string());
    eprintln!("urn2: {}", urn2.to_string());
    
    assert_eq!(urn1.to_string(), urn2.to_string(), "Tag order should be normalized to canonical form");
    assert_eq!(urn1, urn2, "URNs with same tags should be equal");
}
