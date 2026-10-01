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
#[derive(Clone)]
pub struct TaggedUrn {
    /// The prefix for this URN (e.g., "cap", "myapp", "custom")
    prefix: String,
    /// The tags that define this URN, stored in sorted order for canonical representation
    tags: BTreeMap<String, String>,
    /// The same URN on the proved model's side: its tags with the proof that
    /// their keys are strictly increasing. Every semantic question — does one
    /// refine another, are they equivalent, how specific is it — is asked of
    /// this, through code generated from `../formal`, so what this crate
    /// answers is what the theorems there are about.
    ///
    /// Built once, when the URN is; the fields are private so that it can
    /// never describe a different URN from the one beside it.
    formal: FormalUrn,
}

/// A URN as the generated code takes it: a handle, shared by reference count,
/// which passes into the proved functions without conversion. Crates whose own
/// generated code shares the URN type map `TaggedUrn.Exec.Wf` to this.
pub type FormalUrn = lungo::LeanValue<crate::formal::__opaque::Wf>;

impl fmt::Debug for TaggedUrn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TaggedUrn")
            .field("prefix", &self.prefix)
            .field("tags", &self.tags)
            .finish()
    }
}

impl PartialEq for TaggedUrn {
    fn eq(&self, other: &Self) -> bool {
        self.prefix == other.prefix && self.tags == other.tags
    }
}

impl Eq for TaggedUrn {}

impl std::hash::Hash for TaggedUrn {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.prefix.hash(state);
        self.tags.hash(state);
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

/// Order-theoretic classification of the relation between two tagged URNs.
///
/// This is derived from the existing `accepts` / `is_comparable` /
/// `is_equivalent` semantics; it does not replace them. It is attached to
/// coordinate deltas so callers can distinguish same-point edits from
/// same-chain edits and cross-branch edits without pretending that delta is
/// only valid for comparable pairs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TaggedUrnRelationKind {
    Equivalent,
    Comparable,
    Incomparable,
}

/// Coordinate-space edit from one tagged URN to another with the same prefix.
///
/// `removed` contains the exact canonical coordinate entries present in the
/// base but absent or changed in the target. `added` contains the exact
/// canonical coordinate entries absent from the base or changed in the target.
///
/// For a changed key, that key appears in both maps:
/// - removed[key] = old value
/// - added[key]   = new value
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TaggedUrnCoordinateDelta {
    prefix: String,
    pub removed: BTreeMap<String, String>,
    pub added: BTreeMap<String, String>,
    pub relation_kind: TaggedUrnRelationKind,
}

impl TaggedUrnCoordinateDelta {
    pub fn prefix(&self) -> &str {
        &self.prefix
    }

    pub fn is_empty(&self) -> bool {
        self.removed.is_empty() && self.added.is_empty()
    }
}

/// The model's form for a stored tag value (`None` is a key the URN omits).
fn constraint_of(value: Option<&str>) -> crate::formal::Constraint {
    use crate::formal::Constraint as C;
    match value {
        None => C::Missing,
        Some("?") => C::Unconstrained,
        Some("*") => C::Present,
        Some("!") => C::Absent,
        Some(v) if v.starts_with("?=") => C::OptionalNot { value: v[2..].to_string() },
        Some(v) if v.starts_with("!=") => C::PresentNot { value: v[2..].to_string() },
        Some(v) => C::Exact { value: v.to_string() },
    }
}

impl TaggedUrn {
    /// The one way a `TaggedUrn` is made: the Rust fields and the model's
    /// handle, together, from the same tags.
    fn assemble(prefix: String, tags: BTreeMap<String, String>) -> Self {
        let model_tags: lungo::List<(String, crate::formal::Constraint)> = tags
            .iter()
            .map(|(k, v)| (k.clone(), constraint_of(Some(v))))
            .collect();
        let formal = crate::formal::exec::make(prefix.clone(), model_tags)
            // A BTreeMap's keys are strictly increasing, in the order Lean's
            // `String <` uses (UTF-8 byte order is code-point order).
            .expect("a tag map's keys are strictly increasing");
        TaggedUrn { prefix, tags, formal }
    }

    /// Create a new tagged URN from tags with a specified prefix
    /// Keys are normalized to lowercase; values are preserved as-is
    pub fn new(prefix: String, tags: BTreeMap<String, String>) -> Self {
        let normalized_tags = tags
            .into_iter()
            .map(|(k, v)| (k.to_lowercase(), v))
            .collect();
        Self::assemble(prefix.to_lowercase(), normalized_tags)
    }

    /// Create an empty tagged URN with the specified prefix
    pub fn empty(prefix: String) -> Self {
        Self::assemble(prefix.to_lowercase(), BTreeMap::new())
    }

    /// The prefix (`media`, `cap`, …).
    pub fn prefix(&self) -> &str {
        &self.prefix
    }

    /// The tags, in key order, as stored values (`*`, `?`, `!`, `?=v`, `!=v`, or an exact value).
    pub fn tags(&self) -> &BTreeMap<String, String> {
        &self.tags
    }

    /// The model's handle for this URN, for generated code that shares the type.
    pub fn formal(&self) -> &FormalUrn {
        &self.formal
    }

    /// The same prefix with other tags: every edit builds a new URN, handle included.
    fn with_tags(&self, tags: BTreeMap<String, String>) -> Self {
        Self::assemble(self.prefix.clone(), tags)
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
            return Ok(Self::assemble(prefix, tags));
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
                            return Err(TaggedUrnError::EmptyTagComponent("empty key".to_string()));
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
                            return Err(TaggedUrnError::EmptyTagComponent("empty key".to_string()));
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
                    return Err(TaggedUrnError::EmptyTagComponent("empty key".to_string()));
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

        Ok(Self::assemble(prefix, tags))
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
        value
            .chars()
            .any(|c| c == ';' || c == '=' || c == '"' || c == '\\' || c == ' ' || c.is_uppercase())
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
        self.tags
            .iter()
            .map(|(k, v)| {
                match v.as_str() {
                    "*" => k.clone(),         // bare key
                    "?" => format!("?{}", k), // prefix `?k`
                    "!" => format!("!{}", k), // prefix `!k`
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
    pub fn with_tag(self, key: String, value: String) -> Result<Self, TaggedUrnError> {
        if value.is_empty() {
            return Err(TaggedUrnError::EmptyTagComponent(format!(
                "empty value for key '{}' (use '*' for wildcard)",
                key
            )));
        }
        Ok(self.with_tag_unchecked(key, value))
    }

    /// Add or update a tag (infallible version for internal use where value is known valid)
    fn with_tag_unchecked(self, key: String, value: String) -> Self {
        let mut tags = self.tags;
        tags.insert(key.to_lowercase(), value);
        Self::assemble(self.prefix, tags)
    }

    /// Remove a tag
    /// Key is normalized to lowercase for case-insensitive removal
    pub fn without_tag(self, key: &str) -> Self {
        let mut tags = self.tags;
        tags.remove(&key.to_lowercase());
        Self::assemble(self.prefix, tags)
    }

    /// Whether this URN (the instance) satisfies `pattern`: `self ⪯ pattern`.
    ///
    /// Decided by the proved model (`TaggedUrn.Exec.refines`, which
    /// `refines_decides` shows is exactly the specified relation): every tag
    /// form means the set of states it allows — on either side — and the
    /// instance satisfies the pattern when, key by key, its set lies inside
    /// the pattern's. A key the instance omits promises nothing, `?x`
    /// promises nothing, and `x` promises presence but no particular value.
    ///
    /// Both URNs must have the same prefix; comparing across prefixes is a
    /// programming error, reported as `PrefixMismatch`.
    ///
    /// Equivalent to `pattern.accepts(self)`.
    pub fn conforms_to(&self, pattern: &TaggedUrn) -> Result<bool, TaggedUrnError> {
        Self::same_prefix(self, pattern)?;
        Ok(crate::formal::exec::refines(self.formal.clone(), pattern.formal.clone()))
    }

    /// Whether this URN (as a pattern) accepts `instance`: `instance ⪯ self`.
    ///
    /// Equivalent to `instance.conforms_to(self)`.
    pub fn accepts(&self, instance: &TaggedUrn) -> Result<bool, TaggedUrnError> {
        instance.conforms_to(self)
    }

    /// Whether this URN and `other` COULD be about the same thing: some thing
    /// is described by both. Symmetric — neither is the instance.
    ///
    /// `conforms_to` is a guarantee: everything this URN describes, the
    /// pattern describes. This is the other question the same meanings
    /// answer, and the one a search asks: `media:ext` (some ext) does not
    /// conform to `media:ext=pdf`, and it is not excluded by it either — it
    /// meets it, and only the value that turns up says which. Whatever
    /// conforms meets; what meets need not conform, and meeting is not
    /// transitive (a pdf meets "some ext", which meets a png).
    ///
    /// Decided by the proved model (`TaggedUrn.Exec.meets`).
    pub fn meets(&self, other: &TaggedUrn) -> Result<bool, TaggedUrnError> {
        Self::same_prefix(self, other)?;
        Ok(crate::formal::exec::meets(self.formal.clone(), other.formal.clone()))
    }

    /// Whether this URN, read as a COMPLETE thing, satisfies `pattern`.
    ///
    /// A description that omits a key says nothing about it, and that is how
    /// `conforms_to` reads both sides. A thing that exists — a value with
    /// these tags, a cap's own list of tags — omits a key because it does not
    /// have it. Read so, a thing that does not mention `x` satisfies `!x`,
    /// which no description that merely omits `x` does.
    ///
    /// Use this where the left side is what something IS; use `conforms_to`
    /// where it is what something is declared to take or give. Decided by the
    /// proved model (`TaggedUrn.Exec.refinesClosed`).
    pub fn satisfies(&self, pattern: &TaggedUrn) -> Result<bool, TaggedUrnError> {
        Self::same_prefix(self, pattern)?;
        Ok(crate::formal::exec::refines_closed(self.formal.clone(), pattern.formal.clone()))
    }

    /// Whether this URN, read as a complete thing, COULD satisfy `pattern`:
    /// `satisfies` is to this as `conforms_to` is to `meets`. A thing tagged
    /// `ext` (some ext) may satisfy `ext=pdf`; one that does not mention `ext`
    /// may not.
    pub fn may_satisfy(&self, pattern: &TaggedUrn) -> Result<bool, TaggedUrnError> {
        Self::same_prefix(self, pattern)?;
        Ok(crate::formal::exec::meets_closed(self.formal.clone(), pattern.formal.clone()))
    }

    fn same_prefix(instance: &TaggedUrn, pattern: &TaggedUrn) -> Result<(), TaggedUrnError> {
        if instance.prefix != pattern.prefix {
            return Err(TaggedUrnError::PrefixMismatch {
                expected: pattern.prefix.clone(),
                actual: instance.prefix.clone(),
            });
        }
        Ok(())
    }

    /// Classify a stored value into its form, for the tie-break counts of
    /// [`specificity_tuple`](Self::specificity_tuple).
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

    /// One key: does the instance's stored value satisfy the pattern's?
    /// (`None` is a key the URN omits.) The proved model's per-key rule
    /// (`TaggedUrn.Exec.valuesMatch`), exposed for callers such as `CapUrn`'s
    /// cap-tag matcher that walk tag sets themselves.
    pub fn values_match(inst: Option<&str>, patt: Option<&str>) -> bool {
        crate::formal::exec::values_match(constraint_of(inst), constraint_of(patt))
    }

    /// One key: do the two stored values allow a common state?
    /// (`TaggedUrn.Exec.valuesMeet`.)
    pub fn values_meet(a: Option<&str>, b: Option<&str>) -> bool {
        crate::formal::exec::values_meet(constraint_of(a), constraint_of(b))
    }

    /// One key of a complete thing against a pattern: an omitted key is absent.
    /// (`TaggedUrn.Exec.valuesMatchClosed`.)
    pub fn values_match_closed(inst: Option<&str>, patt: Option<&str>) -> bool {
        crate::formal::exec::values_match_closed(constraint_of(inst), constraint_of(patt))
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
        Self::same_prefix(other, self)?;
        Ok(crate::formal::exec::equivalent(self.formal.clone(), other.formal.clone()))
    }

    /// Check if two URNs are comparable (one is a specialization of the other).
    ///
    /// From order theory: in a partial order, two elements are **comparable**
    /// when one is ≤ the other. Elements that are NOT comparable are in
    /// different branches of the specialization lattice (e.g., `media:pdf`
    /// vs `media:enc=utf-8;txt` — neither accepts the other).
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
        Self::same_prefix(other, self)?;
        Ok(crate::formal::exec::comparable(self.formal.clone(), other.formal.clone()))
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

    /// Compute the coordinate-space delta from `base` to `self`.
    ///
    /// This operates on the explicit canonical coordinate representation, not
    /// on quotient-level semantic equivalence. Two URNs that are equivalent
    /// under matching may still produce a non-empty delta if one explicitly
    /// authors additional no-op coordinates.
    pub fn delta_from(&self, base: &TaggedUrn) -> Result<TaggedUrnCoordinateDelta, TaggedUrnError> {
        if self.prefix != base.prefix {
            return Err(TaggedUrnError::PrefixMismatch {
                expected: base.prefix.clone(),
                actual: self.prefix.clone(),
            });
        }

        let relation_kind = if self.is_equivalent(base)? {
            TaggedUrnRelationKind::Equivalent
        } else if self.is_comparable(base)? {
            TaggedUrnRelationKind::Comparable
        } else {
            TaggedUrnRelationKind::Incomparable
        };

        let mut removed = BTreeMap::new();
        let mut added = BTreeMap::new();
        let all_keys: std::collections::BTreeSet<String> =
            base.tags.keys().chain(self.tags.keys()).cloned().collect();

        for key in all_keys {
            let base_value = base.tags.get(&key);
            let target_value = self.tags.get(&key);
            if base_value == target_value {
                continue;
            }
            if let Some(value) = base_value {
                removed.insert(key.clone(), value.clone());
            }
            if let Some(value) = target_value {
                added.insert(key.clone(), value.clone());
            }
        }

        Ok(TaggedUrnCoordinateDelta {
            prefix: self.prefix.clone(),
            removed,
            added,
            relation_kind,
        })
    }

    /// Apply a coordinate delta to this tagged URN.
    ///
    /// Keys named in `removed` are deleted regardless of their current value,
    /// then keys named in `added` are inserted with the target value. Unrelated
    /// coordinates are preserved unchanged.
    pub fn apply_delta(&self, delta: &TaggedUrnCoordinateDelta) -> Result<Self, TaggedUrnError> {
        if self.prefix != delta.prefix {
            return Err(TaggedUrnError::PrefixMismatch {
                expected: delta.prefix.clone(),
                actual: self.prefix.clone(),
            });
        }

        let mut tags = self.tags.clone();
        for key in delta.removed.keys() {
            tags.remove(key);
        }
        for (key, value) in &delta.added {
            tags.insert(key.clone(), value.clone());
        }
        Ok(self.with_tags(tags))
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
        let score = crate::formal::exec::specificity(self.formal.clone());
        score
            .to_u64()
            .expect("a sum of per-tag scores fits in u64") as usize
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
        self.with_tags(tags)
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
        Ok(self.with_tags(tags))
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
    pub fn find_best_match<'a>(
        urns: &'a [TaggedUrn],
        request: &TaggedUrn,
    ) -> Result<Option<&'a TaggedUrn>, TaggedUrnError> {
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
    pub fn find_all_matches<'a>(
        urns: &'a [TaggedUrn],
        request: &TaggedUrn,
    ) -> Result<Vec<&'a TaggedUrn>, TaggedUrnError> {
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
    pub fn are_compatible(
        urns1: &[TaggedUrn],
        urns2: &[TaggedUrn],
    ) -> Result<bool, TaggedUrnError> {
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
        let key_lower = key.to_lowercase();
        if self.tags.contains_key(&key_lower) {
            return Err(TaggedUrnError::DuplicateKey(key_lower));
        }
        let mut validated_key = key_lower;
        let mut validated_value = value.to_string();
        let mut single_tag = BTreeMap::new();
        TaggedUrn::finish_tag(&mut single_tag, &mut validated_key, &mut validated_value)?;
        let (final_key, final_value) = single_tag.into_iter().next().expect(
            "TaggedUrn::finish_tag must insert exactly one validated tag for TaggedUrnBuilder::tag",
        );
        self.tags.insert(final_key, final_value);
        Ok(self)
    }

    /// Add a tag with key (normalized to lowercase) and wildcard value
    pub fn marker(mut self, key: &str) -> Result<Self, TaggedUrnError> {
        let key_lower = key.to_lowercase();
        if self.tags.contains_key(&key_lower) {
            return Err(TaggedUrnError::DuplicateKey(key_lower));
        }
        let mut validated_key = key_lower;
        let mut validated_value = "*".to_string();
        let mut single_tag = BTreeMap::new();
        TaggedUrn::finish_tag(&mut single_tag, &mut validated_key, &mut validated_value)?;
        let (final_key, final_value) = single_tag.into_iter().next().expect(
            "TaggedUrn::finish_tag must insert exactly one validated tag for TaggedUrnBuilder::marker",
        );
        self.tags.insert(final_key, final_value);
        Ok(self)
    }

    pub fn build(self) -> Result<TaggedUrn, TaggedUrnError> {
        if self.tags.is_empty() {
            return Err(TaggedUrnError::Empty);
        }
        Ok(TaggedUrn::assemble(self.prefix, self.tags))
    }

    /// Build allowing empty tags (creates an empty URN that matches everything)
    pub fn build_allow_empty(self) -> TaggedUrn {
        TaggedUrn::assemble(self.prefix, self.tags)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // TEST0501: Create tagged URN from string and verify prefix and tag values
    #[test]
    fn test0501_tagged_urn_creation() {
        let urn =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail;")
                .unwrap();
        assert_eq!(urn.get_prefix(), "cap");
        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.get_tag("target"), Some(&"thumbnail".to_string()));
        assert_eq!(urn.get_tag("ext"), Some(&"pdf".to_string()));
    }

    // TEST0502: Parse URN with custom prefix and verify serialization
    #[test]
    fn test0502_custom_prefix() {
        let urn = TaggedUrn::from_string("myapp:generate;ext=pdf").unwrap();
        assert_eq!(urn.get_prefix(), "myapp");
        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.to_string(), "myapp:ext=pdf;generate");
    }

    // TEST0503: Normalize prefix to lowercase regardless of input case
    #[test]
    fn test0503_prefix_case_insensitive() {
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

    // TEST0504: Return PrefixMismatch error when comparing URNs with different prefixes
    #[test]
    fn test0504_prefix_mismatch_error() {
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

    // TEST0505: Build URN with custom prefix using TaggedUrnBuilder
    #[test]
    fn test0505_builder_with_prefix() {
        let urn = TaggedUrnBuilder::new("custom")
            .tag("key", "value")
            .expect("builder tag fixture must be valid")
            .build()
            .expect("builder fixture must serialize");

        assert_eq!(urn.get_prefix(), "custom");
        assert_eq!(urn.to_string(), "custom:key=value");
    }

    // TEST0506: Normalize unquoted keys and values to lowercase
    #[test]
    fn test0506_unquoted_values_lowercased() {
        // Unquoted values are normalized to lowercase
        let urn =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail;")
                .unwrap();

        // Keys are always lowercase
        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.get_tag("ext"), Some(&"pdf".to_string()));
        assert_eq!(urn.get_tag("target"), Some(&"thumbnail".to_string()));

        // Key lookup is case-insensitive (try uppercase variations of an
        // existing key — `EXT` and `Ext` resolve to the same `ext` value).
        assert_eq!(urn.get_tag("EXT"), Some(&"pdf".to_string()));
        assert_eq!(urn.get_tag("Ext"), Some(&"pdf".to_string()));

        // Both URNs parse to same lowercase values (same tags, same values)
        let urn2 =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail;")
                .unwrap();
        assert_eq!(urn.to_string(), urn2.to_string());
        assert_eq!(urn, urn2);
    }

    // TEST0507: Preserve original case for quoted values while lowercasing keys
    #[test]
    fn test0507_quoted_values_preserve_case() {
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

    // TEST0508: Parse quoted values containing semicolons, equals signs, and spaces
    #[test]
    fn test0508_quoted_value_special_chars() {
        // Semicolons in quoted values
        let urn = TaggedUrn::from_string(r#"cap:key="value;with;semicolons""#).unwrap();
        assert_eq!(
            urn.get_tag("key"),
            Some(&"value;with;semicolons".to_string())
        );

        // Equals in quoted values
        let urn2 = TaggedUrn::from_string(r#"cap:key="value=with=equals""#).unwrap();
        assert_eq!(urn2.get_tag("key"), Some(&"value=with=equals".to_string()));

        // Spaces in quoted values
        let urn3 = TaggedUrn::from_string(r#"cap:key="hello world""#).unwrap();
        assert_eq!(urn3.get_tag("key"), Some(&"hello world".to_string()));
    }

    // TEST0509: Parse escape sequences for quotes and backslashes in quoted values
    #[test]
    fn test0509_quoted_value_escape_sequences() {
        // Escaped quotes
        let urn = TaggedUrn::from_string(r#"cap:key="value\"quoted\"""#).unwrap();
        assert_eq!(urn.get_tag("key"), Some(&r#"value"quoted""#.to_string()));

        // Escaped backslashes
        let urn2 = TaggedUrn::from_string(r#"cap:key="path\\file""#).unwrap();
        assert_eq!(urn2.get_tag("key"), Some(&r#"path\file"#.to_string()));

        // Mixed escapes
        let urn3 = TaggedUrn::from_string(r#"cap:key="say \"hello\\world\"""#).unwrap();
        assert_eq!(
            urn3.get_tag("key"),
            Some(&r#"say "hello\world""#.to_string())
        );
    }

    // TEST0510: Parse URN with both quoted and unquoted tag values
    #[test]
    fn test0510_mixed_quoted_unquoted() {
        let urn = TaggedUrn::from_string(r#"cap:a="Quoted";b=simple"#).unwrap();
        assert_eq!(urn.get_tag("a"), Some(&"Quoted".to_string()));
        assert_eq!(urn.get_tag("b"), Some(&"simple".to_string()));
    }

    // TEST0511: Reject unterminated quoted value with appropriate error
    #[test]
    fn test0511_unterminated_quote_error() {
        let result = TaggedUrn::from_string(r#"cap:key="unterminated"#);
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(matches!(e, TaggedUrnError::UnterminatedQuote(_)));
        }
    }

    // TEST0512: Reject invalid escape sequences in quoted values
    #[test]
    fn test0512_invalid_escape_sequence_error() {
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

    // TEST0513: Apply smart quoting during serialization based on value content
    #[test]
    fn test0513_serialization_smart_quoting() {
        // Simple lowercase value - no quoting needed
        let urn = TaggedUrnBuilder::new("cap")
            .tag("key", "simple")
            .expect("simple key fixture must be valid")
            .build()
            .expect("simple key fixture must serialize");
        assert_eq!(urn.to_string(), "cap:key=simple");

        // Value with spaces - needs quoting
        let urn2 = TaggedUrnBuilder::new("cap")
            .tag("key", "has spaces")
            .expect("spaced value fixture must be valid")
            .build()
            .expect("spaced value fixture must serialize");
        assert_eq!(urn2.to_string(), r#"cap:key="has spaces""#);

        // Value with semicolons - needs quoting
        let urn3 = TaggedUrnBuilder::new("cap")
            .tag("key", "has;semi")
            .expect("semicolon value fixture must be valid")
            .build()
            .expect("semicolon value fixture must serialize");
        assert_eq!(urn3.to_string(), r#"cap:key="has;semi""#);

        // Value with uppercase - needs quoting to preserve
        let urn4 = TaggedUrnBuilder::new("cap")
            .tag("key", "HasUpper")
            .expect("mixed-case value fixture must be valid")
            .build()
            .expect("mixed-case value fixture must serialize");
        assert_eq!(urn4.to_string(), r#"cap:key="HasUpper""#);

        // Value with quotes - needs quoting and escaping
        let urn5 = TaggedUrnBuilder::new("cap")
            .tag("key", r#"has"quote"#)
            .expect("quoted value fixture must be valid")
            .build()
            .expect("quoted value fixture must serialize");
        assert_eq!(urn5.to_string(), r#"cap:key="has\"quote""#);

        // Value with backslashes - needs quoting and escaping
        let urn6 = TaggedUrnBuilder::new("cap")
            .tag("key", r#"path\file"#)
            .expect("backslash value fixture must be valid")
            .build()
            .expect("backslash value fixture must serialize");
        assert_eq!(urn6.to_string(), r#"cap:key="path\\file""#);
    }

    // TEST0514: Round-trip parse and serialize a simple URN
    #[test]
    fn test0514_round_trip_simple() {
        let original = "cap:ext=pdf;generate;in=media:;out=media:";
        let urn = TaggedUrn::from_string(original).unwrap();
        let serialized = urn.to_string();
        let reparsed = TaggedUrn::from_string(&serialized).unwrap();
        assert_eq!(urn, reparsed);
    }

    // TEST0515: Round-trip parse and serialize a URN with quoted values
    #[test]
    fn test0515_round_trip_quoted() {
        let original = r#"cap:key="Value With Spaces""#;
        let urn = TaggedUrn::from_string(original).unwrap();
        let serialized = urn.to_string();
        let reparsed = TaggedUrn::from_string(&serialized).unwrap();
        assert_eq!(urn, reparsed);
        assert_eq!(
            reparsed.get_tag("key"),
            Some(&"Value With Spaces".to_string())
        );
    }

    // TEST0516: Round-trip parse and serialize a URN with escape sequences
    #[test]
    fn test0516_round_trip_escapes() {
        let original = r#"cap:key="value\"with\\escapes""#;
        let urn = TaggedUrn::from_string(original).unwrap();
        assert_eq!(
            urn.get_tag("key"),
            Some(&r#"value"with\escapes"#.to_string())
        );
        let serialized = urn.to_string();
        let reparsed = TaggedUrn::from_string(&serialized).unwrap();
        assert_eq!(urn, reparsed);
    }

    // TEST0517: Require a prefix in URN string and reject missing prefix
    #[test]
    fn test0517_prefix_required() {
        // Missing prefix should fail
        assert!(TaggedUrn::from_string("generate;ext=pdf").is_err());

        // Valid prefix should work
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        assert!(urn.has_marker_tag("generate"));

        // Case-insensitive prefix
        let urn2 = TaggedUrn::from_string("CAP:generate").unwrap();
        assert!(urn2.has_marker_tag("generate"));
    }

    // TEST0518: Treat trailing semicolon as equivalent to no trailing semicolon
    #[test]
    fn test0518_trailing_semicolon_equivalence() {
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

    // TEST0519: Serialize tags in alphabetical order as canonical string format
    #[test]
    fn test0519_canonical_string_format() {
        let urn =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail")
                .unwrap();
        // Should be sorted alphabetically and have no trailing semicolon in canonical form
        // Alphabetical order: ext < op < target
        assert_eq!(
            urn.to_string(),
            "cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail"
        );
    }

    // TEST0520: Match tags with exact values, subsets, wildcards, and mismatches
    #[test]
    fn test0520_tag_matching() {
        let urn =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail;")
                .unwrap();

        // Exact match
        let request1 =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;target=thumbnail;")
                .unwrap();
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

    // TEST0521: Enforce case-sensitive matching for quoted tag values
    #[test]
    fn test0521_matching_case_sensitive_values() {
        // Values with different case should NOT match
        let urn1 = TaggedUrn::from_string(r#"cap:key="Value""#).unwrap();
        let urn2 = TaggedUrn::from_string(r#"cap:key="value""#).unwrap();
        assert!(!urn1.conforms_to(&urn2).unwrap());
        assert!(!urn2.conforms_to(&urn1).unwrap());

        // Same case should match
        let urn3 = TaggedUrn::from_string(r#"cap:key="Value""#).unwrap();
        assert!(urn1.conforms_to(&urn3).unwrap());
    }

    // TEST0522: Handle missing tags in instance vs pattern matching semantics
    #[test]
    fn test0522_missing_tag_handling() {
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

    // TEST0523: Compute graded specificity scores and tuples for URN tags
    #[test]
    fn test0523_specificity() {
        // Six-form per-tag specificity ladder:
        //   ?x        : 0  (no constraint)
        //   x?=v      : 1  (absent OR not v)
        //   x (=x=*)  : 2  (must-have-any)
        //   x!=v      : 3  (present and not v)
        //   x=v       : 4  (must-have-this-value)
        //   !x        : 5  (must-not-have)

        let urn1 = TaggedUrn::from_string("cap:general").unwrap(); // bare marker -> 2
        let urn2 = TaggedUrn::from_string("cap:ext=pdf").unwrap(); // exact -> 4
        let urn3 = TaggedUrn::from_string("cap:gen;ext=pdf").unwrap(); // marker + exact = 2+4
        let urn4 = TaggedUrn::from_string("cap:?ext").unwrap(); // ?x -> 0
        let urn5 = TaggedUrn::from_string("cap:!ext").unwrap(); // !x -> 5
        let urn6 = TaggedUrn::from_string("cap:ext?=pdf").unwrap(); // x?=v -> 1
        let urn7 = TaggedUrn::from_string("cap:ext!=pdf").unwrap(); // x!=v -> 3

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

    // TEST0524: Build URN with multiple tags using TaggedUrnBuilder
    #[test]
    fn test0524_builder() {
        let urn = TaggedUrnBuilder::new("cap")
            .marker("generate")
            .expect("generate marker fixture must be valid")
            .tag("target", "thumbnail")
            .expect("target fixture must be valid")
            .tag("ext", "pdf")
            .expect("ext fixture must be valid")
            .tag("output", "binary")
            .expect("output fixture must be valid")
            .build()
            .expect("builder fixture must serialize");

        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.get_tag("output"), Some(&"binary".to_string()));
    }

    // TEST0525: Preserve value case in builder while lowercasing keys
    #[test]
    fn test0525_builder_preserves_case() {
        let urn = TaggedUrnBuilder::new("cap")
            .tag("KEY", "ValueWithCase")
            .expect("case-preserving fixture must be valid")
            .build()
            .expect("case-preserving fixture must serialize");

        // Key is lowercase
        assert_eq!(urn.get_tag("key"), Some(&"ValueWithCase".to_string()));
        // Value case preserved, so needs quoting
        assert_eq!(urn.to_string(), r#"cap:key="ValueWithCase""#);
    }

    // TEST0526: Verify directional accepts between patterns with shared and disjoint tags
    #[test]
    fn test0526_directional_accepts_with_tag_overlap() {
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
        let full_instance =
            TaggedUrn::from_string("cap:ext=pdf;format=png;generate;in=media:;out=media:").unwrap();
        assert!(specific.accepts(&full_instance).unwrap());
        assert!(wildcard.accepts(&full_instance).unwrap());
    }

    // TEST0527: Find best matching URN by specificity from a list of candidates
    #[test]
    fn test0527_best_match() {
        let urns = vec![
            TaggedUrn::from_string("cap:op").unwrap(),
            TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap(),
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap(),
        ];

        let request = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();
        let best = UrnMatcher::find_best_match(&urns, &request)
            .unwrap()
            .unwrap();

        // Most specific URN that can handle the request
        // Alphabetical order: ext < op
        assert_eq!(
            best.to_string(),
            "cap:ext=pdf;generate;in=media:;out=media:"
        );
    }

    // TEST0528: Merge two URNs and extract a subset of tags
    #[test]
    fn test0528_merge_and_subset() {
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

    // TEST0529: Reject merge of URNs with different prefixes
    #[test]
    fn test0529_merge_prefix_mismatch() {
        let urn1 = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();
        let urn2 = TaggedUrn::from_string("myapp:ext=pdf").unwrap();

        let result = urn1.merge(&urn2);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            TaggedUrnError::PrefixMismatch { .. }
        ));
    }

    // TEST0530: Convert specific tag value to wildcard and verify matching behavior
    #[test]
    fn test0530_wildcard_tag() {
        let urn = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        let wildcarded = urn.clone().with_wildcard_tag("ext");

        // Wildcard serializes as value-less tag
        assert_eq!(wildcarded.to_string(), "cap:ext");

        // Test that wildcarded URN can match more requests
        let request = TaggedUrn::from_string("cap:ext=jpg").unwrap();
        assert!(!urn.conforms_to(&request).unwrap());
        assert!(wildcarded
            .conforms_to(&TaggedUrn::from_string("cap:ext").unwrap())
            .unwrap());
    }

    // TEST0531: Handle empty tagged URN with no tags in matching and serialization
    #[test]
    fn test0531_empty_tagged_urn() {
        // Empty tagged URN is valid
        let empty_urn = TaggedUrn::from_string("cap:").unwrap();
        assert_eq!(empty_urn.tags.len(), 0);
        assert_eq!(empty_urn.to_string(), "cap:");

        // NEW SEMANTICS:
        // Empty PATTERN matches any INSTANCE (pattern has no constraints)
        // Empty INSTANCE only matches patterns that have no required tags

        let specific_urn =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();

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

    // TEST0532: Create empty URN with custom prefix
    #[test]
    fn test0532_empty_with_custom_prefix() {
        let empty_urn = TaggedUrn::from_string("myapp:").unwrap();
        assert_eq!(empty_urn.get_prefix(), "myapp");
        assert_eq!(empty_urn.tags.len(), 0);
        assert_eq!(empty_urn.to_string(), "myapp:");
    }

    // TEST0533: Parse forward slashes and colons in unquoted tag values
    #[test]
    fn test0533_extended_character_support() {
        // Test forward slashes and colons in tag components
        let urn =
            TaggedUrn::from_string("cap:url=https://example_org/api;path=/some/file").unwrap();
        assert_eq!(
            urn.get_tag("url"),
            Some(&"https://example_org/api".to_string())
        );
        assert_eq!(urn.get_tag("path"), Some(&"/some/file".to_string()));
    }

    // TEST0534: Reject wildcard in keys but accept wildcard in values
    #[test]
    fn test0534_wildcard_restrictions() {
        // Wildcard should be rejected in keys
        assert!(TaggedUrn::from_string("cap:*=value").is_err());

        // Wildcard should be accepted in values
        let urn = TaggedUrn::from_string("cap:key=*").unwrap();
        assert_eq!(urn.get_tag("key"), Some(&"*".to_string()));
    }

    // TEST0535: Reject duplicate keys in URN string
    #[test]
    fn test0535_duplicate_key_rejection() {
        let result = TaggedUrn::from_string("cap:key=value1;key=value2");
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(matches!(e, TaggedUrnError::DuplicateKey(_)));
        }
    }

    // TEST0536: Reject purely numeric keys but allow mixed alphanumeric keys
    #[test]
    fn test0536_numeric_key_restriction() {
        // Pure numeric keys should be rejected
        assert!(TaggedUrn::from_string("cap:123=value").is_err());

        // Mixed alphanumeric keys should be allowed
        assert!(TaggedUrn::from_string("cap:key123=value").is_ok());
        assert!(TaggedUrn::from_string("cap:123key=value").is_ok());

        // Pure numeric values should be allowed
        assert!(TaggedUrn::from_string("cap:key=123").is_ok());
    }

    // TEST0537: Reject empty value after equals sign
    #[test]
    fn test0537_empty_value_error() {
        assert!(TaggedUrn::from_string("cap:key=").is_err());
        assert!(TaggedUrn::from_string("cap:key=;other=value").is_err());
    }

    // TEST0538: Verify has_tag uses case-sensitive value comparison and case-insensitive key lookup
    #[test]
    fn test0538_has_tag_case_sensitive() {
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

    // TEST0539: Preserve value case when adding tag with with_tag method
    #[test]
    fn test0539_with_tag_preserves_value() {
        let urn = TaggedUrn::empty("cap".to_string())
            .with_tag("key".to_string(), "ValueWithCase".to_string())
            .unwrap();
        assert_eq!(urn.get_tag("key"), Some(&"ValueWithCase".to_string()));
    }

    // TEST0540: Reject empty value string in with_tag method
    #[test]
    fn test0540_with_tag_rejects_empty_value() {
        let result =
            TaggedUrn::empty("cap".to_string()).with_tag("key".to_string(), "".to_string());
        assert!(result.is_err());
        if let Err(TaggedUrnError::EmptyTagComponent(msg)) = result {
            assert!(msg.contains("empty value"));
        } else {
            panic!("Expected EmptyTagComponent error");
        }
    }

    // TEST0541: Reject empty value string in builder tag method
    #[test]
    fn test0541_builder_rejects_empty_value() {
        let result = TaggedUrnBuilder::new("cap").tag("key", "");
        assert!(result.is_err());
        if let Err(TaggedUrnError::EmptyTagComponent(msg)) = result {
            assert!(msg.contains("empty value"));
        } else {
            panic!("Expected EmptyTagComponent error");
        }
    }

    // TEST0542: Treat quoted and unquoted simple lowercase values as semantically equivalent
    #[test]
    fn test0542_semantic_equivalence() {
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

    // TEST0543: Verify exact match when instance and pattern have identical tags
    #[test]
    fn test0543_matching_semantics_test1_exact_match() {
        // Test 1: Exact match
        // URN:     cap:ext=pdf;generate;in=media:;out=media:
        // Request: cap:ext=pdf;generate;in=media:;out=media:
        // Result:  MATCH
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let request = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        assert!(
            urn.conforms_to(&request).unwrap(),
            "Test 1: Exact match should succeed"
        );
    }

    // TEST0544: Reject match when instance is missing a tag required by pattern
    #[test]
    fn test0544_matching_semantics_test2_instance_missing_tag() {
        // Test 2: Instance missing tag
        // Instance: cap:generate;in=media:;out=media:
        // Pattern:  cap:ext=pdf;generate;in=media:;out=media:
        // Result:   NO MATCH (pattern requires ext=pdf, instance doesn't have ext)
        //
        // NEW SEMANTICS: Missing tag in instance means it doesn't exist.
        // Pattern K=v requires instance to have K=v.
        let instance = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();
        let pattern = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        assert!(
            !instance.conforms_to(&pattern).unwrap(),
            "Test 2: Instance missing tag should NOT match when pattern requires it"
        );

        // To accept any ext (or missing), use pattern with ext=?
        let pattern_optional =
            TaggedUrn::from_string("cap:ext=?;generate;in=media:;out=media:").unwrap();
        assert!(
            instance.conforms_to(&pattern_optional).unwrap(),
            "Pattern with ext=? should match instance without ext"
        );
    }

    // TEST0545: Match when instance has extra tags not constrained by pattern
    #[test]
    fn test0545_matching_semantics_test3_urn_has_extra_tag() {
        // Test 3: URN has extra tag
        // URN:     cap:ext=pdf;generate;in=media:;out=media:;version=2
        // Request: cap:ext=pdf;generate;in=media:;out=media:
        // Result:  MATCH (request doesn't constrain version)
        let urn =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:;version=2").unwrap();
        let request = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        assert!(
            urn.conforms_to(&request).unwrap(),
            "Test 3: URN with extra tag should match"
        );
    }

    // TEST0546: Match when pattern has wildcard accepting any value for a tag
    #[test]
    fn test0546_matching_semantics_test4_request_has_wildcard() {
        // Test 4: Request has wildcard
        // URN:     cap:ext=pdf;generate;in=media:;out=media:
        // Request: cap:ext;generate;in=media:;out=media:
        // Result:  MATCH (request accepts any ext)
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let request = TaggedUrn::from_string("cap:ext;generate;in=media:;out=media:").unwrap();
        assert!(
            urn.conforms_to(&request).unwrap(),
            "Test 4: Request wildcard should match"
        );
    }

    // TEST0547: An instance's wildcard promises presence, not the value asked for
    //
    // `ext` is "some ext". It used to satisfy a pattern asking for `ext=pdf` —
    // "decided later" — which let a cap promising some ext stand in for one that
    // produces a pdf. A pdf satisfies "some ext"; "some ext" does not satisfy pdf.
    #[test]
    fn test0547_matching_semantics_test5_urn_has_wildcard() {
        let urn = TaggedUrn::from_string("cap:ext;generate;in=media:;out=media:").unwrap();
        let request = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        assert!(!urn.conforms_to(&request).unwrap(), "some ext does not satisfy ext=pdf");
        assert!(request.conforms_to(&urn).unwrap(), "ext=pdf satisfies some ext");
        // Not a guarantee, and not excluded: some ext COULD be a pdf. That is
        // `meets`, which is where "decided later" belongs.
        assert!(urn.meets(&request).unwrap(), "some ext could be ext=pdf");
        assert!(request.meets(&urn).unwrap(), "meeting has no direction");
    }

    // TEST0548: Reject match when tag values conflict between instance and pattern
    #[test]
    fn test0548_matching_semantics_test6_value_mismatch() {
        // Test 6: Value mismatch
        // URN:     cap:ext=pdf;generate;in=media:;out=media:
        // Request: cap:ext=docx;generate;in=media:;out=media:
        // Result:  NO MATCH
        let urn = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let request = TaggedUrn::from_string("cap:ext=docx;generate;in=media:;out=media:").unwrap();
        assert!(
            !urn.conforms_to(&request).unwrap(),
            "Test 6: Value mismatch should not match"
        );
    }

    // TEST0549: Reject match when pattern requires a tag absent from instance
    #[test]
    fn test0549_matching_semantics_test7_pattern_has_extra_tag() {
        // Test 7: Pattern has extra tag that instance doesn't have
        // Instance: cap:generate_thumbnail;in=media:;out=media:binary
        // Pattern:  cap:ext=wav;generate_thumbnail;in=media:;out=media:binary
        // Result:   NO MATCH (pattern requires ext=wav, instance doesn't have ext)
        //
        // NEW SEMANTICS: Pattern K=v requires instance to have K=v
        let instance =
            TaggedUrn::from_string(r#"cap:generate_thumbnail;in=media:;out=media:binary"#).unwrap();
        let pattern =
            TaggedUrn::from_string(r#"cap:ext=wav;generate_thumbnail;in=media:;out=media:binary"#)
                .unwrap();
        assert!(
            !instance.conforms_to(&pattern).unwrap(),
            "Test 7: Instance missing ext should NOT match when pattern requires ext=wav"
        );

        // Instance vs pattern that doesn't constrain ext: MATCH
        let pattern_no_ext =
            TaggedUrn::from_string(r#"cap:generate_thumbnail;in=media:;out=media:binary"#).unwrap();
        assert!(instance.conforms_to(&pattern_no_ext).unwrap());
    }

    // TEST0550: Match any instance against empty pattern with no constraints
    #[test]
    fn test0550_matching_semantics_test8_empty_pattern_matches_anything() {
        // Test 8: Empty PATTERN matches any INSTANCE
        // Instance: cap:ext=pdf;generate;in=media:;out=media:
        // Pattern:  cap:
        // Result:   MATCH (pattern has no constraints)
        //
        // NEW SEMANTICS: Empty pattern = no constraints = matches any instance
        // But empty instance only matches patterns that don't require tags
        let instance = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let empty_pattern = TaggedUrn::from_string("cap:").unwrap();
        assert!(
            instance.conforms_to(&empty_pattern).unwrap(),
            "Test 8: Any instance should match empty pattern"
        );

        // Empty instance vs pattern with requirements: NO MATCH
        let empty_instance = TaggedUrn::from_string("cap:").unwrap();
        let pattern = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        assert!(
            !empty_instance.conforms_to(&pattern).unwrap(),
            "Empty instance should NOT match pattern with requirements"
        );
    }

    // TEST0551: Reject match when instance and pattern have non-overlapping tag dimensions
    #[test]
    fn test0551_matching_semantics_test9_cross_dimension_constraints() {
        // Test 9: Cross-dimension constraints
        // Instance: cap:generate;in=media:;out=media:
        // Pattern:  cap:ext=pdf
        // Result:   NO MATCH (pattern requires ext=pdf, instance doesn't have ext)
        //
        // NEW SEMANTICS: Pattern K=v requires instance to have K=v
        let instance = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();
        let pattern = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        assert!(
            !instance.conforms_to(&pattern).unwrap(),
            "Test 9: Instance without ext should NOT match pattern requiring ext"
        );

        // Instance with ext vs pattern with different tag only: MATCH
        let instance2 =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let pattern2 = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        assert!(
            instance2.conforms_to(&pattern2).unwrap(),
            "Instance with ext=pdf should match pattern requiring ext=pdf"
        );
    }

    // TEST0552: Return error for conforms_to, accepts, and is_more_specific_than with different prefixes
    #[test]
    fn test0552_matching_different_prefixes_error() {
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

    // TEST0553: Parse single value-less tag as wildcard
    #[test]
    fn test0553_valueless_tag_parsing_single() {
        // Single value-less tag
        let urn = TaggedUrn::from_string("cap:optimize").unwrap();
        assert_eq!(urn.get_tag("optimize"), Some(&"*".to_string()));
        // Serializes as value-less (no =*)
        assert_eq!(urn.to_string(), "cap:optimize");
    }

    // TEST0554: Parse multiple value-less tags and serialize alphabetically
    #[test]
    fn test0554_valueless_tag_parsing_multiple() {
        // Multiple value-less tags
        let urn = TaggedUrn::from_string("cap:fast;optimize;secure").unwrap();
        assert_eq!(urn.get_tag("fast"), Some(&"*".to_string()));
        assert_eq!(urn.get_tag("optimize"), Some(&"*".to_string()));
        assert_eq!(urn.get_tag("secure"), Some(&"*".to_string()));
        // Serializes alphabetically as value-less
        assert_eq!(urn.to_string(), "cap:fast;optimize;secure");
    }

    // TEST0555: Parse mix of value-less and valued tags together
    #[test]
    fn test0555_valueless_tag_mixed_with_valued() {
        // Mix of value-less and valued tags
        let urn =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;optimize;out=media:;secure")
                .unwrap();
        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.get_tag("optimize"), Some(&"*".to_string()));
        assert_eq!(urn.get_tag("ext"), Some(&"pdf".to_string()));
        assert_eq!(urn.get_tag("secure"), Some(&"*".to_string()));
        // Serializes alphabetically
        assert_eq!(
            urn.to_string(),
            "cap:ext=pdf;generate;in=media:;optimize;out=media:;secure"
        );
    }

    // TEST0556: Parse value-less tag at end of URN without trailing semicolon
    #[test]
    fn test0556_valueless_tag_at_end() {
        // Value-less tag at the end (no trailing semicolon)
        let urn = TaggedUrn::from_string("cap:generate;in=media:;optimize;out=media:").unwrap();
        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.get_tag("optimize"), Some(&"*".to_string()));
        assert_eq!(
            urn.to_string(),
            "cap:generate;in=media:;optimize;out=media:"
        );
    }

    // TEST0557: Verify value-less tag is equivalent to explicit wildcard (key=*)
    #[test]
    fn test0557_valueless_tag_equivalence_to_wildcard() {
        // Value-less tag is equivalent to explicit wildcard
        let valueless = TaggedUrn::from_string("cap:ext").unwrap();
        let wildcard = TaggedUrn::from_string("cap:ext=*").unwrap();
        assert_eq!(valueless, wildcard);
        // Both serialize to value-less form
        assert_eq!(valueless.to_string(), "cap:ext");
        assert_eq!(wildcard.to_string(), "cap:ext");
    }

    // TEST0558: A valueless tag promises presence, not a value
    //
    // Reading `ext` as "whatever the pattern wants" made `ext` and `ext=pdf`
    // refine each other — equivalent — and refinement non-transitive. Refinement
    // is inclusion of what each form allows: every pdf is some ext.
    #[test]
    fn test0558_valueless_tag_matching() {
        let urn = TaggedUrn::from_string("cap:ext;generate;in=media:;out=media:").unwrap();

        let request_pdf =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let request_docx =
            TaggedUrn::from_string("cap:ext=docx;generate;in=media:;out=media:").unwrap();
        let request_any =
            TaggedUrn::from_string("cap:ext=anything;generate;in=media:;out=media:").unwrap();

        assert!(!urn.conforms_to(&request_pdf).unwrap(), "some ext is not a promise of pdf");
        assert!(!urn.conforms_to(&request_docx).unwrap(), "some ext is not a promise of docx");
        assert!(!urn.conforms_to(&request_any).unwrap(), "nor of any particular value");
        assert!(request_pdf.conforms_to(&urn).unwrap(), "a pdf is some ext");
        assert!(!urn.is_equivalent(&request_pdf).unwrap(), "ext and ext=pdf are different tag sets");
        // It could be any of them, and that is all it is: pdf and docx each
        // meet "some ext" and do not meet each other.
        assert!(urn.meets(&request_pdf).unwrap() && urn.meets(&request_docx).unwrap());
        assert!(!request_pdf.meets(&request_docx).unwrap(), "meeting is not transitive");
    }

    // TEST0559: Require value-less tag in pattern to be present in instance
    #[test]
    fn test0559_valueless_tag_in_pattern() {
        // Pattern with value-less tag (K=*) requires instance to have the tag
        let pattern = TaggedUrn::from_string("cap:ext;generate;in=media:;out=media:").unwrap();

        let instance_pdf =
            TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let instance_docx =
            TaggedUrn::from_string("cap:ext=docx;generate;in=media:;out=media:").unwrap();
        let instance_missing = TaggedUrn::from_string("cap:generate;in=media:;out=media:").unwrap();

        // NEW SEMANTICS: K=* (valueless tag) means must-have-any
        assert!(instance_pdf.conforms_to(&pattern).unwrap()); // Has ext=pdf
        assert!(instance_docx.conforms_to(&pattern).unwrap()); // Has ext=docx
        assert!(!instance_missing.conforms_to(&pattern).unwrap()); // Missing ext, pattern requires it

        // To accept missing ext, use ? instead
        let pattern_optional =
            TaggedUrn::from_string("cap:ext=?;generate;in=media:;out=media:").unwrap();
        assert!(instance_missing.conforms_to(&pattern_optional).unwrap());
    }

    // TEST0560: Score value-less wildcard tags with graded specificity
    #[test]
    fn test0560_valueless_tag_specificity() {
        // Six-form ladder: ?x=0, x?=v=1, x=*=2, x!=v=3, x=v=4, !x=5.
        let urn1 = TaggedUrn::from_string("cap:generate").unwrap(); // 1 marker
        let urn2 = TaggedUrn::from_string("cap:generate;optimize").unwrap(); // 2 markers
        let urn3 = TaggedUrn::from_string("cap:ext=pdf;generate").unwrap(); // 1 marker + 1 exact

        assert_eq!(urn1.specificity(), 2); // 1 marker = 2
        assert_eq!(urn2.specificity(), 4); // 2 markers = 2 + 2 = 4
        assert_eq!(urn3.specificity(), 6); // 1 marker + 1 exact = 2 + 4 = 6
    }

    // TEST0561: Round-trip value-less tags through parse and serialize
    #[test]
    fn test0561_valueless_tag_roundtrip() {
        // Round-trip parsing and serialization
        let original = "cap:ext=pdf;generate;in=media:;optimize;out=media:;secure";
        let urn = TaggedUrn::from_string(original).unwrap();
        let serialized = urn.to_string();
        let reparsed = TaggedUrn::from_string(&serialized).unwrap();
        assert_eq!(urn, reparsed);
        assert_eq!(serialized, original);
    }

    // TEST0562: Normalize value-less tag keys to lowercase
    #[test]
    fn test0562_valueless_tag_case_normalization() {
        // Value-less tags are normalized to lowercase like other keys
        let urn = TaggedUrn::from_string("cap:OPTIMIZE;Fast;SECURE").unwrap();
        assert_eq!(urn.get_tag("optimize"), Some(&"*".to_string()));
        assert_eq!(urn.get_tag("fast"), Some(&"*".to_string()));
        assert_eq!(urn.get_tag("secure"), Some(&"*".to_string()));
        assert_eq!(urn.to_string(), "cap:fast;optimize;secure");
    }

    // TEST0563: Reject empty value with equals sign as distinct from value-less tag
    #[test]
    fn test0563_empty_value_still_error() {
        // Empty value with = is still an error (different from value-less)
        assert!(TaggedUrn::from_string("cap:key=").is_err());
        assert!(TaggedUrn::from_string("cap:key=;other=value").is_err());
    }

    // TEST0564: Verify directional accepts of value-less wildcard tags with specific values
    #[test]
    fn test0564_valueless_tag_directional_accepts() {
        // Value-less tags stored as * act as pattern requiring any present value
        let wildcard_ext = TaggedUrn::from_string("cap:ext;generate;in=media:;out=media:").unwrap();
        let ext_pdf = TaggedUrn::from_string("cap:ext=pdf;generate;in=media:;out=media:").unwrap();
        let ext_docx =
            TaggedUrn::from_string("cap:ext=docx;generate;in=media:;out=media:").unwrap();

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

    // TEST0565: Reject purely numeric keys for value-less tags
    #[test]
    fn test0565_valueless_numeric_key_still_rejected() {
        // Purely numeric keys are still rejected for value-less tags
        assert!(TaggedUrn::from_string("cap:123").is_err());
        assert!(TaggedUrn::from_string("cap:generate;in=media:;out=media:;456").is_err());
    }

    // TEST0566: Reject leading, trailing, and embedded whitespace in URN input
    #[test]
    fn test0566_whitespace_in_input_rejected() {
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

    // TEST0567: Parse question mark as unspecified value and verify
    // serialization. All three input aliases (?x, x?, x=?) parse to
    // the same stored value `"?"` and serialize as the canonical
    // prefix form `?x`.
    #[test]
    fn test0567_unspecified_question_mark_parsing() {
        let urn = TaggedUrn::from_string("cap:ext=?").unwrap();
        assert_eq!(urn.get_tag("ext"), Some(&"?".to_string()));
        // Canonical form is `?ext` (prefix), not `ext=?`.
        assert_eq!(urn.to_string(), "cap:?ext");
    }

    // TEST0568: Parse exclamation mark as must-not-have value and
    // verify serialization. All three input aliases (!x, x!, x=!)
    // parse to stored value `"!"` and serialize as canonical `!x`.
    #[test]
    fn test0568_must_not_have_exclamation_parsing() {
        let urn = TaggedUrn::from_string("cap:ext=!").unwrap();
        assert_eq!(urn.get_tag("ext"), Some(&"!".to_string()));
        // Canonical form is `!ext` (prefix), not `ext=!`.
        assert_eq!(urn.to_string(), "cap:!ext");
    }

    // TEST0569: Match any instance against pattern with unspecified (?) tag value
    #[test]
    fn test0569_question_mark_pattern_matches_anything() {
        // Pattern with K=? matches any instance (with or without K)
        let pattern = TaggedUrn::from_string("cap:ext=?").unwrap();

        let instance_pdf = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        let instance_docx = TaggedUrn::from_string("cap:ext=docx").unwrap();
        let instance_missing = TaggedUrn::from_string("cap:").unwrap();
        let instance_wildcard = TaggedUrn::from_string("cap:ext=*").unwrap();
        let instance_must_not = TaggedUrn::from_string("cap:ext=!").unwrap();

        assert!(
            instance_pdf.conforms_to(&pattern).unwrap(),
            "ext=pdf should match ext=?"
        );
        assert!(
            instance_docx.conforms_to(&pattern).unwrap(),
            "ext=docx should match ext=?"
        );
        assert!(
            instance_missing.conforms_to(&pattern).unwrap(),
            "(no ext) should match ext=?"
        );
        assert!(
            instance_wildcard.conforms_to(&pattern).unwrap(),
            "ext=* should match ext=?"
        );
        assert!(
            instance_must_not.conforms_to(&pattern).unwrap(),
            "ext=! should match ext=?"
        );
    }

    // TEST0570: An instance with K=? promises nothing about K
    //
    // `?` is "no constraint" on either side. As an instance it used to satisfy
    // every pattern, which made refinement non-transitive:
    // missing ⪯ ?k ⪯ k=v, yet missing ⋠ k=v. It satisfies exactly the patterns
    // that ask for nothing.
    #[test]
    fn test0570_question_mark_in_instance() {
        let instance = TaggedUrn::from_string("cap:ext=?").unwrap();

        let pattern_pdf = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        let pattern_wildcard = TaggedUrn::from_string("cap:ext=*").unwrap();
        let pattern_must_not = TaggedUrn::from_string("cap:ext=!").unwrap();
        let pattern_question = TaggedUrn::from_string("cap:ext=?").unwrap();
        let pattern_missing = TaggedUrn::from_string("cap:").unwrap();

        assert!(!instance.conforms_to(&pattern_pdf).unwrap(), "ext=? promises no pdf");
        assert!(!instance.conforms_to(&pattern_wildcard).unwrap(), "ext=? promises no presence");
        assert!(!instance.conforms_to(&pattern_must_not).unwrap(), "ext=? promises no absence");
        // It excludes nothing either: it could be any of them.
        assert!(instance.meets(&pattern_pdf).unwrap());
        assert!(instance.meets(&pattern_wildcard).unwrap());
        assert!(instance.meets(&pattern_must_not).unwrap());
        assert!(
            instance.conforms_to(&pattern_question).unwrap(),
            "ext=? should match ext=?"
        );
        assert!(
            instance.conforms_to(&pattern_missing).unwrap(),
            "ext=? should match (no ext)"
        );
    }

    // TEST0571: Pattern with K=! requires the instance to SAY K is absent
    //
    // A key an instance does not mention is not a promise that it is absent:
    // as a pattern the same omission means "anything", and one form cannot
    // mean two things. `media:pdf` satisfied `media:pdf;!compressed` while
    // `media:pdf;compressed` satisfied `media:pdf` and not the `!compressed`
    // pattern, so refinement was not transitive.
    #[test]
    fn test0571_must_not_have_pattern_requires_absent() {
        let pattern = TaggedUrn::from_string("cap:ext=!").unwrap();

        let instance_missing = TaggedUrn::from_string("cap:").unwrap();
        let instance_pdf = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        let instance_wildcard = TaggedUrn::from_string("cap:ext=*").unwrap();
        let instance_must_not = TaggedUrn::from_string("cap:ext=!").unwrap();

        assert!(
            !instance_missing.conforms_to(&pattern).unwrap(),
            "(no ext) does not promise ext is absent"
        );
        // A THING that does not mention ext does not have it: read as complete
        // — which is what a value, or a cap's own tag list, is — it satisfies
        // the pattern. A description that omits ext could go either way.
        assert!(
            instance_missing.satisfies(&pattern).unwrap(),
            "a complete thing with no ext satisfies ext=!"
        );
        assert!(instance_missing.meets(&pattern).unwrap());
        assert!(
            !instance_pdf.conforms_to(&pattern).unwrap(),
            "ext=pdf should NOT match ext=!"
        );
        assert!(
            !instance_wildcard.conforms_to(&pattern).unwrap(),
            "ext=* should NOT match ext=!"
        );
        assert!(
            instance_must_not.conforms_to(&pattern).unwrap(),
            "ext=! should match ext=!"
        );
    }

    // TEST0572: Reject instance with must-not-have (!) tag against patterns requiring that tag
    #[test]
    fn test0572_must_not_have_in_instance() {
        // Instance with K=! conflicts with patterns requiring K
        let instance = TaggedUrn::from_string("cap:ext=!").unwrap();

        let pattern_pdf = TaggedUrn::from_string("cap:ext=pdf").unwrap();
        let pattern_wildcard = TaggedUrn::from_string("cap:ext=*").unwrap();
        let pattern_must_not = TaggedUrn::from_string("cap:ext=!").unwrap();
        let pattern_question = TaggedUrn::from_string("cap:ext=?").unwrap();
        let pattern_missing = TaggedUrn::from_string("cap:").unwrap();

        assert!(
            !instance.conforms_to(&pattern_pdf).unwrap(),
            "ext=! should NOT match ext=pdf"
        );
        assert!(
            !instance.conforms_to(&pattern_wildcard).unwrap(),
            "ext=! should NOT match ext=*"
        );
        assert!(
            instance.conforms_to(&pattern_must_not).unwrap(),
            "ext=! should match ext=!"
        );
        assert!(
            instance.conforms_to(&pattern_question).unwrap(),
            "ext=! should match ext=?"
        );
        assert!(
            instance.conforms_to(&pattern_missing).unwrap(),
            "ext=! should match (no ext)"
        );
    }

    // TEST0573: Verify full cross-product truth table for all instance/pattern value combinations
    #[test]
    fn test0573_full_cross_product_matching() {
        // Each form means the set of states it allows, on either side, and an
        // instance satisfies a pattern when its set is inside the pattern's
        // (../formal, `tagMatch_iff_allows`).

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
        check("cap:", "cap:k=!", false, "(none)/K=!");
        check("cap:", "cap:k", false, "(none)/K=*"); // K is valueless = *
        check("cap:", "cap:k=v", false, "(none)/K=v");

        // Instance K=?, Pattern variations
        check("cap:k=?", "cap:", true, "K=?/(none)");
        check("cap:k=?", "cap:k=?", true, "K=?/K=?");
        check("cap:k=?", "cap:k=!", false, "K=?/K=!");
        check("cap:k=?", "cap:k", false, "K=?/K=*");
        check("cap:k=?", "cap:k=v", false, "K=?/K=v");

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
        check("cap:k", "cap:k=v", false, "K=*/K=v");

        // Instance K=v, Pattern variations
        check("cap:k=v", "cap:", true, "K=v/(none)");
        check("cap:k=v", "cap:k=?", true, "K=v/K=?");
        check("cap:k=v", "cap:k=!", false, "K=v/K=!");
        check("cap:k=v", "cap:k", true, "K=v/K=*");
        check("cap:k=v", "cap:k=v", true, "K=v/K=v");
        check("cap:k=v", "cap:k=w", false, "K=v/K=w");
    }

    // TEST0574: Match URN with mixed required, optional, forbidden, and exact tags
    #[test]
    fn test0574_mixed_special_values() {
        // Test URNs with multiple special values
        let pattern =
            TaggedUrn::from_string("cap:required;optional=?;forbidden=!;exact=pdf").unwrap();

        // Instance that satisfies all constraints — including stating that the
        // forbidden key is absent, which leaving it out does not promise.
        let good_instance =
            TaggedUrn::from_string("cap:required=yes;optional=maybe;forbidden=!;exact=pdf").unwrap();
        assert!(good_instance.conforms_to(&pattern).unwrap());
        let silent_on_forbidden =
            TaggedUrn::from_string("cap:required=yes;optional=maybe;exact=pdf").unwrap();
        assert!(
            !silent_on_forbidden.conforms_to(&pattern).unwrap(),
            "saying nothing about forbidden is not saying it is absent"
        );

        // Instance missing required tag
        let missing_required = TaggedUrn::from_string("cap:optional=maybe;exact=pdf").unwrap();
        assert!(!missing_required.conforms_to(&pattern).unwrap());

        // Instance has forbidden tag
        let has_forbidden =
            TaggedUrn::from_string("cap:required=yes;forbidden=oops;exact=pdf").unwrap();
        assert!(!has_forbidden.conforms_to(&pattern).unwrap());

        // Instance with wrong exact value
        let wrong_exact = TaggedUrn::from_string("cap:required=yes;exact=doc").unwrap();
        assert!(!wrong_exact.conforms_to(&pattern).unwrap());
    }

    // TEST0575: Round-trip all special values (?, !, *, exact) through parse and serialize
    #[test]
    fn test0575_serialization_round_trip_special_values() {
        // All special values round-trip correctly
        let originals = [
            "cap:ext=?",
            "cap:ext=!",
            "cap:ext", // * serializes as valueless
            "cap:a=?;b=!;c;d=exact",
        ];

        for original in originals {
            let urn = TaggedUrn::from_string(original).unwrap();
            let serialized = urn.to_string();
            let reparsed = TaggedUrn::from_string(&serialized).unwrap();
            assert_eq!(urn, reparsed, "Round-trip failed for: {}", original);
        }
    }

    // TEST0576: Check bidirectional accepts between !, *, ?, and specific value tags
    #[test]
    fn test0576_bidirectional_accepts_with_special_values() {
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
        assert!(
            unspecified.accepts(&must_have).unwrap() || must_have.accepts(&unspecified).unwrap()
        );
        assert!(unspecified.accepts(&specific).unwrap() || specific.accepts(&unspecified).unwrap());
        assert!(
            unspecified.accepts(&unspecified).unwrap()
                || unspecified.accepts(&unspecified).unwrap()
        );
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
        let txt = TaggedUrn::from_string("media:enc=utf-8;txt").unwrap();
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
        assert!(!urn.is_comparable_str("media:enc=utf-8;txt").unwrap()); // different branch
    }

    // TEST586: Special values (*, !, ?) with is_equivalent and is_comparable
    #[test]
    fn test586_special_values() {
        let must_have = TaggedUrn::from_string("cap:ext").unwrap(); // ext=*
        let exact = TaggedUrn::from_string("cap:ext=pdf").unwrap(); // ext=pdf
        let must_not = TaggedUrn::from_string("cap:ext=!").unwrap(); // ext=!
        let unspecified = TaggedUrn::from_string("cap:ext=?").unwrap(); // ext=?

        // must_have (*) and exact (pdf): comparable — a pdf is some ext — and
        // NOT equivalent: equivalence is "the same tag set", and they are not
        // (../formal, `equivalent_iff_same_forms`). Reading * as "whatever the
        // pattern wants" made them equivalent, and equivalence not transitive.
        assert!(!must_have.is_equivalent(&exact).unwrap());
        assert!(must_have.is_comparable(&exact).unwrap());

        // must_not (!) and exact (pdf): incomparable (conflict both directions)
        assert!(!must_not.is_comparable(&exact).unwrap());
        assert!(!must_not.is_equivalent(&exact).unwrap());

        // must_not (!) and must_have (*): incomparable (conflict both directions)
        assert!(!must_not.is_comparable(&must_have).unwrap());
        assert!(!must_not.is_equivalent(&must_have).unwrap());

        // unspecified (?) accepts everything, and is equivalent only to what
        // also constrains nothing: every form refines it, it refines none.
        assert!(!unspecified.is_equivalent(&exact).unwrap());
        assert!(!unspecified.is_equivalent(&must_have).unwrap());
        assert!(!unspecified.is_equivalent(&must_not).unwrap());
        assert!(unspecified.is_comparable(&exact).unwrap());
        assert!(unspecified.is_comparable(&must_not).unwrap());
        assert!(unspecified.is_equivalent(&TaggedUrn::from_string("cap:").unwrap()).unwrap());
    }

    // TEST0577: Verify graded specificity scores and tuples for special value types
    // under the six-form ladder.
    #[test]
    fn test0577_specificity_with_special_values() {
        // Six-form ladder: ?x=0, x?=v=1, x=*=2, x!=v=3, x=v=4, !x=5
        let exact = TaggedUrn::from_string("cap:a=x;b=y;c=z").unwrap(); // 3 * 4 = 12
        let must_have = TaggedUrn::from_string("cap:a;b;c").unwrap(); // 3 * 2 = 6
        let must_not = TaggedUrn::from_string("cap:!a;!b;!c").unwrap(); // 3 * 5 = 15
        let unspecified = TaggedUrn::from_string("cap:?a;?b;?c").unwrap(); // 3 * 0 = 0
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
            .expect("generate marker fixture must be valid")
            .tag("target", "thumbnail")
            .expect("target fixture must be valid")
            .tag("format", "pdf")
            .expect("format fixture must be valid")
            .tag("output", "binary")
            .expect("output fixture must be valid")
            .build()
            .expect("builder fixture must serialize");

        assert!(urn.has_marker_tag("generate"));
        assert_eq!(urn.get_tag("target"), Some(&"thumbnail".to_string()));
        assert_eq!(urn.get_tag("format"), Some(&"pdf".to_string()));
        assert_eq!(urn.get_tag("output"), Some(&"binary".to_string()));
    }

    // TEST588: Builder with custom tags
    #[test]
    fn test588_builder_custom_tags() {
        let urn = TaggedUrnBuilder::new("cap")
            .tag("engine", "v2")
            .expect("engine fixture must be valid")
            .tag("quality", "high")
            .expect("quality fixture must be valid")
            .marker("compress")
            .expect("compress marker fixture must be valid")
            .build()
            .expect("builder fixture must serialize");

        assert_eq!(urn.get_tag("engine"), Some(&"v2".to_string()));
        assert_eq!(urn.get_tag("quality"), Some(&"high".to_string()));
        assert!(urn.has_marker_tag("compress"));
    }

    // TEST589: Builder tag overrides (last value wins)
    #[test]
    fn test589_builder_tag_overrides() {
        let urn = TaggedUrnBuilder::new("cap")
            .marker("convert")
            .expect("convert marker fixture must be valid")
            .tag("format", "jpg")
            .expect("format fixture must be valid")
            .build()
            .expect("builder fixture must serialize");

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
            .tag("type", "utility")
            .expect("single-tag fixture must be valid")
            .build()
            .expect("single-tag fixture must serialize");

        assert_eq!(urn.to_string(), "cap:type=utility");
        assert_eq!(urn.get_tag("type"), Some(&"utility".to_string()));
        // Six-form ladder: exact value = 4 points.
        assert_eq!(urn.specificity(), 4);
    }

    // TEST592: Builder with complex multi-tag URN
    #[test]
    fn test592_builder_complex() {
        let urn = TaggedUrnBuilder::new("cap")
            .tag("type", "media")
            .expect("type fixture must be valid")
            .marker("transcode")
            .expect("transcode marker fixture must be valid")
            .tag("target", "video")
            .expect("target fixture must be valid")
            .tag("format", "mp4")
            .expect("format fixture must be valid")
            .tag("codec", "h264")
            .expect("codec fixture must be valid")
            .tag("quality", "1080p")
            .expect("quality fixture must be valid")
            .tag("framerate", "30fps")
            .expect("framerate fixture must be valid")
            .tag("output", "binary")
            .expect("output fixture must be valid")
            .build()
            .expect("builder fixture must serialize");

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
            .expect("convert marker fixture must be valid")
            .marker("ext")
            .expect("ext marker fixture must be valid")
            .marker("quality")
            .expect("quality marker fixture must be valid")
            .build()
            .expect("builder fixture must serialize");

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
            .tag("key", "value")
            .unwrap()
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
            .tag("op", "generate")
            .unwrap()
            .tag("target", "thumbnail")
            .unwrap()
            .tag("format", "pdf")
            .unwrap()
            .build()
            .unwrap();

        // Create a more general pattern (fewer constraints)
        let general_pattern = TaggedUrnBuilder::new("cap")
            .tag("op", "generate")
            .unwrap()
            .build()
            .unwrap();

        // Create a pattern with wildcard (ext=* means must-have-any)
        let wildcard_pattern = TaggedUrnBuilder::new("cap")
            .tag("op", "generate")
            .unwrap()
            .tag("target", "thumbnail")
            .unwrap()
            .tag("ext", "*")
            .unwrap()
            .build()
            .unwrap();

        // Specific instance should match general pattern (pattern has fewer constraints)
        assert!(specific_instance.conforms_to(&general_pattern).unwrap());

        // NEW SEMANTICS: wildcardPattern has ext=* which means instance MUST have ext
        // specificInstance doesn't have ext, so this should NOT match
        assert!(!specific_instance.conforms_to(&wildcard_pattern).unwrap());

        // Check specificity
        assert!(specific_instance
            .is_more_specific_than(&general_pattern)
            .unwrap());

        // Six-form ladder: exact = 4 points, * (must-have-any) = 2 points.
        assert_eq!(specific_instance.specificity(), 12); // 3 exact × 4 = 12
        assert_eq!(general_pattern.specificity(), 4); // 1 exact × 4 = 4
        assert_eq!(wildcard_pattern.specificity(), 10); // 2 exact × 4 + 1 * × 2 = 8 + 2 = 10
    }
}

// TEST0001: Tag order normalization
#[test]
fn test0001_tag_order_normalization() {
    // Two URNs with same tags in different order should produce identical canonical string
    let urn1 = TaggedUrn::from_string("media:list;enc=utf-8").unwrap();
    let urn2 = TaggedUrn::from_string("media:enc=utf-8;list").unwrap();

    eprintln!("urn1: {}", urn1.to_string());
    eprintln!("urn2: {}", urn2.to_string());

    assert_eq!(
        urn1.to_string(),
        urn2.to_string(),
        "Tag order should be normalized to canonical form"
    );
    assert_eq!(urn1, urn2, "URNs with same tags should be equal");
}
