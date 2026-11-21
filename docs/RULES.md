# Tagged URN Rules

## Definitive specification for Tagged URN format and behavior

### 1. Case Handling
- **Prefix and tag keys:** Always normalized to lowercase
- **Unquoted values:** Normalized to lowercase
- **Quoted values:** Case is preserved exactly as specified
- Example: `cap:key=VALUE` stores the exact value `value` (lowercase)
- Example: `cap:key="VALUE"` stores the exact value `VALUE` (preserved)

### 2. Tag Order Independence
The order of tags in the URN string does not matter. Tags are always sorted alphabetically by key in canonical form.

### 3. Mandatory Prefix
A tagged URN begins with a prefix (an identifier: `cap`, `media`, or any other) and `:`. The prefix is lowered on parsing.

### 4. Tag Separator
Tags are separated by semicolons (`;`).

### 5. Trailing Semicolon Optional
Presence or absence of the final trailing semicolon does not matter. Both `cap:key=value` and `cap:key=value;` are equivalent.

### 6. Character Restrictions

**Keys and the prefix:** an ASCII letter, then ASCII letters, digits and `-`; lowered.

**Unquoted values:** any characters except `;`, `=`, `"`, `\` and whitespace; lowered.

**Quoted values:**
- Values may be enclosed in double quotes: `key="value"`
- ANY character is allowed inside quoted values (including spaces, semicolons, equals signs)
- Escape sequences inside quotes: `\"` for literal quote, `\\` for literal backslash
- Only `\"` and `\\` are valid escape sequences; other backslash sequences are errors
- Example: `cap:key="value with spaces"` is valid
- Example: `cap:key="value;with=special"` is valid
- Example: `cap:key="quote: \"hello\""` stores `quote: "hello"`

### 7. Tag Structure
- Tag separator within a tag: `=` separates tag key from tag value
- Tag separator between tags: `;` separates key-value pairs
- After the prefix, colons (`:`) are treated as normal characters, not separators

### 8. No Special Tags
No reserved tag names; a key is any identifier.

### 9. Canonical Form and Serialization
- Tags are sorted alphabetically by key
- No final trailing semicolon
- Tag keys are always lowercase
- Each form prints as:
  - `*` (must-have-any) → `k`
  - `?` (no constraint) → `?k`
  - `!` (must-not-have) → `!k`
  - `?=v` → `k?=v`
  - `!=v` → `k!=v`
  - exact `v` → `k=v`
- **Smart quoting on serialization:** a value is quoted when it holds a character a bare value cannot (`;`, `=`, `"`, `\`, whitespace), an uppercase letter, or when it begins with `*`, `?` or `!`; any other value is written bare
- Examples:
  - the exact value `simple` serializes to `cap:key=simple`
  - the exact value `Has Upper` serializes to `cap:key="Has Upper"`
  - the exact value `has;special` serializes to `cap:key="has;special"`
  - the exact value `*` serializes to `cap:key="*"`

### 10. Special Pattern Values
The following forms control matching behavior:

| Authored aliases             | Canonical form | Meaning                             |
|------------------------------|----------------|-------------------------------------|
| `?x` ≡ `x?` ≡ `x=?`          | `?x`           | no constraint                       |
| `?x=v` ≡ `x?=v` ≡ `x=?v`     | `x?=v`         | absent OR (present and not v)       |
| `x` ≡ `x=*`                  | `x`            | present with any value (must-have)  |
| `!x=v` ≡ `x!=v` ≡ `x=!v`     | `x!=v`         | present and not v                   |
| `x=v`                        | `x=v`          | present and exactly v               |
| `!x` ≡ `x!` ≡ `x=!`          | `!x`           | absent (must-not-have)              |
| (missing)                    |                | no constraint                       |

- Special values `*`, `?`, and `!` are accepted only in tag values, not keys
- A form means the same set of states on the instance side and on the pattern side

### 11. Value-less Tags (Must-Have-Any)
- Tags may be specified without a value: `cap:key1=value1;optimize;key2=value2`
- A value-less tag like `optimize` is equivalent to `optimize=*` (must-have-any)
- This asserts that the tag MUST be present (with any value)
- **Parsing:** `tag` (no `=`) is parsed as `tag=*`
- **Serialization:** `tag=*` is serialized as just `tag` (no `=*`)
- **Note:** `tag=` (explicit `=` with no value) is still an error

## Matching Semantics (CRITICAL)

This section defines how Tagged URN matching works when comparing URNs. This is the core algorithm that ALL implementations MUST follow exactly.

### 12. Per-Tag Value Matching

Each form means the set of states its key may be in; an instance conforms to a pattern at a key when every state the instance allows, the pattern allows.

**Truth Table** (instance row against pattern column):

| Inst ↓ \ Pat → | missing | `?x` | `x?=p`  | `x` | `x!=p`  | `x=p`   | `!x` |
|----------------|:-------:|:----:|:-------:|:---:|:-------:|:-------:|:----:|
| **missing**    | ✓       | ✓    | ✗       | ✗   | ✗       | ✗       | ✗    |
| **`?x`**       | ✓       | ✓    | ✗       | ✗   | ✗       | ✗       | ✗    |
| **`x?=v`**     | ✓       | ✓    | v = p   | ✗   | ✗       | ✗       | ✗    |
| **`x`**        | ✓       | ✓    | ✗       | ✓   | ✗       | ✗       | ✗    |
| **`x!=v`**     | ✓       | ✓    | v = p   | ✓   | v = p   | ✗       | ✗    |
| **`x=v`**      | ✓       | ✓    | v ≠ p   | ✓   | v ≠ p   | v = p   | ✗    |
| **`!x`**       | ✓       | ✓    | ✓       | ✗   | ✗       | ✗       | ✓    |

### 13. Matching: `conforms_to` and `accepts`

Matching is **directional** — it matters which URN is the pattern and which is the instance.
Two methods make this explicit:

- `instance.conforms_to(pattern)` — does the instance satisfy the pattern's constraints?
- `pattern.accepts(instance)` — does the pattern accept this instance?

These are equivalent: `a.conforms_to(b)` = `b.accepts(a)`.

Given an **instance** and a **pattern**, the instance conforms to the pattern if and only if:

1. Collect all keys from both instance and pattern
2. For each key, check if the values match using the truth table above
3. If all keys match, return true; otherwise return false

Decided by the proved model (`TaggedUrn.Exec.refines`), per key by the table above.

### 14. Matching Examples

```
# Example 1: Exact match
Instance: cap:generate;ext=pdf
Pattern:  cap:generate;ext=pdf
Result:   MATCH

# Example 2: Pattern has no constraint on ext
Instance: cap:generate;ext=pdf
Pattern:  cap:generate
Result:   MATCH (missing tag = no constraint)

# Example 3: Instance has extra tags
Instance: cap:generate;ext=pdf;version=2
Pattern:  cap:generate;ext=pdf
Result:   MATCH (pattern doesn't constrain version)

# Example 4: Pattern uses must-have-any
Instance: cap:generate;ext=pdf
Pattern:  cap:generate;ext=*
Result:   MATCH (instance has ext, pattern just wants any ext)

# Example 5: Value mismatch
Instance: cap:generate;ext=pdf
Pattern:  cap:generate;ext=docx
Result:   NO MATCH (pdf ≠ docx)

# Example 6: Must-not-have against an instance that omits the key
Instance: cap:generate;in=media:;out=media:
Pattern:  cap:generate;debug=!
Result:   NO MATCH for conforms_to (an instance that omits debug promises nothing)
          MATCH for satisfies, which reads the instance as a complete thing

# Example 7: Must-not-have violated
Instance: cap:generate;debug=true
Pattern:  cap:generate;debug=!
Result:   NO MATCH (instance has debug, pattern wants it absent)

# Example 8: Must-have-any not satisfied
Instance: cap:generate;in=media:;out=media:
Pattern:  cap:generate;ext=*
Result:   NO MATCH (instance lacks ext, pattern requires it)

# Example 9: Explicit unspecified
Instance: cap:generate;ext=pdf
Pattern:  cap:generate;ext=?
Result:   MATCH (pattern explicitly doesn't care about ext)

# Example 10: Value-less tag (must-have-any)
Instance: cap:generate;ext=pdf
Pattern:  cap:generate;ext           (ext = ext=*)
Result:   MATCH (instance has ext with value)
```

### 15. Graded Specificity
When multiple URNs match a request, select the one with highest specificity using graded scoring:

| Form | Score | Example |
|------|-------|---------|
| No constraint (`?x`) or missing | 0 | `?ext` |
| `x?=v` | 1 | `ext?=pdf` |
| Must-have-any (`x`) | 2 | `ext=*` or `ext` |
| `x!=v` | 3 | `ext!=pdf` |
| Exact value (`x=v`) | 4 | `ext=pdf` |
| Must-not-have (`!x`) | 5 | `!debug` |

**Total specificity** = sum of all tag scores

Examples:
- `cap:format=mp4;ext=pdf` → 4 + 4 = 8 (two exact-valued tags)
- `cap:format=mp4;ext=*` → 4 + 2 = 6 (one exact, one must-have-any)
- `cap:format=mp4;ext` → 4 + 2 = 6 (value-less marker = must-have-any)
- `cap:generate` → 2 (one marker = must-have-any)

### 16. Selection Algorithm
1. Collect the URNs that conform to the request.
2. Take the highest specificity.
3. Of equal scores, the first in the list wins (`UrnMatcher::find_best_match`).

### 17. Complete things
`satisfies` reads the instance as a complete thing, so a key it does not mention is absent: `cap:` satisfies `cap:debug=!` and does not conform to it (TEST0571). Decided by the proved model (`TaggedUrn.Exec.refinesClosed`).

### 18. Duplicate Keys
Duplicate keys in the same URN result in an error - last occurrence does not win.

### 19. UTF-8 Support
Keys and prefixes are ASCII; values may hold any Unicode character as the quoting rules allow.

### 20. Numeric Values
A key begins with a letter, so it is never numeric; values may be numeric.

### 21. Empty Tagged URN
`cap:` with no tags is valid and represents no constraints (matches any URN with the same prefix).

### 22. Length Restrictions
No explicit length restrictions, though practical limits exist based on URL and system constraints (typically ~2000 characters).

### 23. Special Value Restrictions
`*`, `?` and `!` are sigils only unquoted; quoted they are literal values.

### 24. Colon Treatment
Forward slashes (`/`) and colons (`:`) are valid in values and treated as normal characters, except for the prefix, which is not part of the tag structure.

### 25. Quote Errors
- **Unterminated Quote:** A quoted value that starts with `"` but never closes is an error
- **Invalid Escape Sequence:** Inside a quoted value, `\` followed by anything other than `"` or `\` is an error
- Examples of errors:
  - `cap:key="unterminated` → UnterminatedQuote error
  - `cap:key="bad\n"` → InvalidEscapeSequence error (only `\"` and `\\` allowed)

### 26. Semantic Equivalence
- `cap:key=simple` and `cap:key="simple"` both parse to the exact value `simple`
- `cap:key="Simple"` parses to the exact value `Simple` (preserved) - NOT equal to unquoted
- A value is stored as the constraint it writes; a quoted value is an exact (or qualified) literal; printing quotes what needs quoting.

## Implementation Notes

- All implementations must normalize the prefix and tag keys to lowercase
- All implementations must normalize unquoted values to lowercase
- All implementations must preserve case in quoted values
- All implementations must sort tags alphabetically in canonical output
- All implementations must handle trailing semicolons consistently
- All implementations must validate character restrictions identically
- All implementations must implement matching logic identically per the truth table
- All implementations must reject duplicate keys with appropriate error messages
- All implementations must use state-machine parsing for quoted value support
- All implementations must implement smart quoting on serialization
- All implementations must parse value-less tags as must-have-any (`tag` → `tag=*`)
- All implementations must serialize must-have-any tags as value-less (`tag=*` → `tag`)
- All implementations must serialize `?` and `!` as `?tag` and `!tag`
- All implementations must implement graded specificity scoring
- All implementations must allow `?` and `!` as unquoted values

## Error Codes (Consistent Across All Implementations)

| Code | Name | Description |
|------|------|-------------|
| 1 | Empty | Empty URN |
| 2 | EmptyTagComponent | Empty value after `=`, or a qualifier with no key |
| 3 | InvalidCharacter | A character not allowed where it stands |
| 4 | InvalidTagFormat | Tag not in key=value format |
| 5 | MissingPrefix | No prefix (no `:`) |
| 6 | DuplicateKey | Same key appears twice |
| 8 | UnterminatedQuote | Quoted value never closed |
| 9 | InvalidEscapeSequence | Invalid escape in quoted value |
| 10 | EmptyPrefix | Empty prefix (`:` at the start) |
| 11 | PrefixMismatch | Two URNs compared across different prefixes |
| 12 | WhitespaceInInput | Leading or trailing whitespace |
