# Tagged URN - Rust Implementation

A tagged URN system with flat tag-based naming, pattern matching, and specificity comparison.

## Overview

Tagged URN provides a formal system for tag-based identifiers with matching and comparison capabilities. It uses a flat key-value tag format that supports special pattern values, graded specificity ranking, and symmetric matching.

## Features

- **Flat Tag Format** - Simple `key=value` pairs separated by semicolons
- **Special Pattern Values** - `*` (must-have-any), `?` (unspecified), `!` (must-not-have)
- **Value-less Tags** - Tags without values (`tag`) are must-have-any (`tag=*`)
- **Case Insensitive** - All input normalized to lowercase (except quoted values)
- **Tag Order Independent** - Canonical alphabetical sorting
- **Graded Specificity** - Exact values score higher than wildcards
- **Smart Quoting** - Automatic quoting for special characters and case preservation

## Format

Tagged URNs use a flat tag-based format: `cap:tag1=value1;tag2=value2;tag3=value3`

**Examples:**
```
cap:extract;target=metadata;ext=pdf
cap:image;format=png;quality=high
cap:generate;target=thumbnail;output=binary
```

**Special Values:**
| Value | Meaning | Example |
|-------|---------|---------|
| `K=v` | Must have key K with exact value v | `format=pdf` |
| `K=*` | Must have key K with any value | `format=*` |
| `K=!` | Must NOT have key K | `debug=!` |
| `K=?` | No constraint on key K | `format=?` |
| (missing) | No constraint (same as `?`) | |

**Value-less Tags:**
- Tags without values mean must-have-any: `cap:extract;optimize` equals `cap:extract;optimize=*`
- Useful for asserting tag presence: `cap:premium;feature=export`

**Specificity:**
- Graded scoring: exact value (3) > must-have-any (2) > must-not-have (1) > unspecified (0)
- `cap:extract;ext=pdf` (specificity 6) is more specific than `cap:extract;ext=*` (specificity 5)

## Usage

```rust
use tagged_urn::{TaggedUrn, TaggedUrnBuilder};

// Create from string
let urn = TaggedUrn::from_string("cap:extract;target=metadata")?;

// Build with builder pattern
let urn = TaggedUrnBuilder::new()
    .tag("op", "extract")
    .tag("target", "metadata")
    .tag("ext", "pdf")
    .build()?;

// Check matching - does instance conform to pattern's constraints?
let pattern = TaggedUrn::from_string("cap:extract")?;
if urn.conforms_to(&pattern)? {
    println!("URN conforms to pattern");
}

// Compare specificity
if urn.is_more_specific_than(&request) {
    println!("URN is more specific");
}
```

## Key Operations

### Matching

```rust
let instance = TaggedUrn::from_string("cap:extract;target=metadata;ext=pdf")?;
let pattern = TaggedUrn::from_string("cap:extract")?;

// instance.conforms_to(pattern) - does instance satisfy pattern's constraints?
if instance.conforms_to(&pattern)? {
    println!("Instance satisfies pattern");
}

// pattern.accepts(instance) - equivalent, from pattern's perspective
if pattern.accepts(&instance)? {
    println!("Pattern accepts instance");
}
```

`conforms_to` is a guarantee: everything the instance describes, the pattern
describes. Two more questions are asked of the same meanings:

```rust
let some_ext = TaggedUrn::from_string("media:ext")?;
let pdf = TaggedUrn::from_string("media:ext=pdf")?;

// "Some ext" is not guaranteed to be a pdf, and it could be one.
assert!(!some_ext.conforms_to(&pdf)?);
assert!(some_ext.meets(&pdf)?);

// A description that omits a key says nothing about it; a complete thing —
// a value's media, a cap's own tags — does not have it.
let plain = TaggedUrn::from_string("media:ext=pdf")?;
let uncompressed = TaggedUrn::from_string("media:ext=pdf;!compressed")?;
assert!(!plain.conforms_to(&uncompressed)?);
assert!(plain.satisfies(&uncompressed)?);
```

The rules are proved in `../formal` (Lean), and this crate runs code generated
from them.

### Specificity Comparison

```rust
let general = TaggedUrn::from_string("cap:extract")?;
let specific = TaggedUrn::from_string("cap:extract;ext=pdf")?;

if specific.is_more_specific_than(&general) {
    println!("Specific URN preferred");
}
```

### Builder Pattern

```rust
let urn = TaggedUrnBuilder::new()
    .tag("inference")
    .tag("op", "conversation")
    .tag("language", "en")
    .build()?;
```

## Cross-Language Compatibility

This Rust implementation produces identical results to:
- [Go implementation](https://github.com/machinefabric/tagged-urn-go)
- [JavaScript implementation](https://github.com/machinefabric/tagged-urn-js)
- [Objective-C implementation](https://github.com/machinefabric/tagged-urn-objc)

All implementations pass the same test cases and follow identical rules.

## Testing

```bash
cargo test
```

## Documentation

See [RULES.md](docs/RULES.md) for the definitive specification of Tagged URN format, syntax, and behavior.

## Dependencies

- `serde` - Serialization/deserialization
- `serde_json` - JSON support
- `regex` - Pattern validation

## License

MIT License
