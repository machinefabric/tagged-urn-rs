# Tagged URN - Rust Implementation

A tagged URN system with flat tag-based naming, pattern matching, and specificity comparison.

## Overview

Tagged URN provides a formal system for tag-based identifiers with matching and comparison capabilities. It uses a flat tag format in which each tag writes a constraint on its key, graded specificity ranking, and refinement decided by a proved model.

## Features

- **Flat Tag Format** - Tags separated by semicolons after a prefix: `prefix:key=value;marker`
- **Six Tag Forms** - exact `k=v`, present `k`, absent `!k`, unconstrained `?k`, `k?=v` (absent or not v), `k!=v` (present and not v)
- **Typed Tags** - Each tag is stored as the constraint it writes (`tagged_urn::Constraint`)
- **Identifiers** - The prefix and every key are an ASCII letter followed by ASCII letters, digits and `-`
- **Case Insensitive** - The prefix, keys and unquoted values are lowered; quoted values keep their case
- **Literal Quoting** - A quoted value is always the literal value: `x="*"` is the exact value `*`
- **Tag Order Independent** - Canonical alphabetical sorting
- **Graded Specificity** - `?k` 0, `k?=v` 1, `k` 2, `k!=v` 3, `k=v` 4, `!k` 5
- **Round Trip** - Every URN prints so that it reads back as itself

## Format

Tagged URNs use a flat tag-based format: `cap:tag1=value1;tag2=value2;tag3`

**Examples:**
```
cap:extract;target=metadata;ext=pdf
cap:image;format=png;quality=high
cap:generate;target=thumbnail;output=binary
```

**Tag forms:**
| Written | Aliases | Constraint | Meaning |
|---------|---------|------------|---------|
| `k=v` | | `Exact { value: v }` | Must have key k with exact value v |
| `k` | `k=*` | `Present` | Must have key k with any value |
| `!k` | `k!`, `k=!` | `Absent` | Must NOT have key k |
| `?k` | `k?`, `k=?` | `Unconstrained` | No constraint on key k |
| `k?=v` | `?k=v`, `k=?v` | `OptionalNot { value: v }` | Absent, or present and not v |
| `k!=v` | `!k=v`, `k=!v` | `PresentNot { value: v }` | Present and not v |
| (missing) | | | No constraint |

**Values:** an unquoted value takes any character but `;`, `=`, `"`, `\` and whitespace, and is lowered. A value that holds one of those, an uppercase letter to keep, or begins with `*`, `?` or `!` is written quoted, and a quoted value is literal.

**Specificity:**
- The sum of the per-tag scores: `cap:extract;ext=pdf` scores 6 (marker 2 + exact 4); `cap:extract;ext` scores 4.

## Usage

```rust
use tagged_urn::{Constraint, TaggedUrn, TaggedUrnBuilder};

// Create from string
let urn = TaggedUrn::from_string("cap:extract;target=metadata")?;
assert_eq!(urn.get_tag("target"), Some(&Constraint::Exact { value: "metadata".into() }));
assert!(urn.has_marker_tag("extract"));

// Build with builder pattern
let urn = TaggedUrnBuilder::new("cap")?
    .marker("extract")?
    .tag("target", "metadata")?
    .tag("ext", "pdf")?
    .build()?;

// Check matching - does instance conform to pattern's constraints?
let pattern = TaggedUrn::from_string("cap:extract")?;
if urn.conforms_to(&pattern)? {
    println!("URN conforms to pattern");
}

// Compare specificity
if urn.is_more_specific_than(&pattern)? {
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
let pdf = TaggedUrn::from_string("media:document;ext=pdf")?;

// "Some ext" is not guaranteed to be a pdf, and it could be one.
assert!(!some_ext.conforms_to(&pdf)?);
assert!(some_ext.meets(&pdf)?);

// A description that omits a key says nothing about it; a complete thing —
// a value's media, a cap's own tags — does not have it.
let plain = TaggedUrn::from_string("media:document;ext=pdf")?;
let uncompressed = TaggedUrn::from_string("media:document;ext=pdf;!compressed")?;
assert!(!plain.conforms_to(&uncompressed)?);
assert!(plain.satisfies(&uncompressed)?);
```

The rules are proved in `../formal` (Lean), and this crate runs code generated
from them.

### The bound of several URNs

```rust
let png = TaggedUrn::from_string("media:ext=png;image")?;
let jpeg = TaggedUrn::from_string("media:ext=jpeg;image")?;

// The most specific URN both conform to: an ext, some value, and an image.
let bound = TaggedUrn::least_upper_bound("media", &[png, jpeg])?;
assert_eq!(bound, TaggedUrn::from_string("media:ext;image")?);
```

Where no single form allows exactly what the inputs allow together, the bound
is the least one that excludes only values the inputs name. It is made by the
proved model too.

### Specificity Comparison

```rust
let general = TaggedUrn::from_string("cap:extract")?;
let specific = TaggedUrn::from_string("cap:extract;ext=pdf")?;

if specific.is_more_specific_than(&general)? {
    println!("Specific URN preferred");
}
```

### Builder Pattern

```rust
let urn = TaggedUrnBuilder::new("cap")?
    .marker("inference")?
    .tag("op", "conversation")?
    .tag("language", "en")?
    .constraint("debug", Constraint::Absent)?
    .build()?;
assert_eq!(urn.to_string(), "cap:!debug;inference;language=en;op=conversation");
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
- `lungo` - The runtime the code generated from the proved model runs on

## License

MIT License
