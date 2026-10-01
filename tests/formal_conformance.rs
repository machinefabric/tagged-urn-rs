//! TEST599: this crate answers what the proved model answers.
//!
//! The matching is generated from `../formal`, so this is not a test of the
//! rules — those are proved — but of everything around them: the parser, the
//! stored-value encoding, and how a `TaggedUrn` hands itself to the generated
//! code. Every row of `../formal/conformance.json` (written by the model,
//! `lake exe conformance`) is parsed by this crate's parser and must get the
//! model's verdict — for the guarantee (`conforms_to`), the possibility
//! (`meets`), and the complete reading of the instance (`satisfies`,
//! `may_satisfy`). The same table runs in every mirror.

use tagged_urn::TaggedUrn;

#[test]
fn test599_every_row_of_the_model_s_table() {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../formal/conformance.json");
    let table: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(path).expect("the model's table"))
            .expect("json");
    let parse = |s: &serde_json::Value| TaggedUrn::from_string(s.as_str().unwrap()).unwrap();
    let mut wrong = Vec::new();
    let rows = table["refines"].as_array().unwrap();
    for r in rows {
        let (a, b) = (parse(&r["instance"]), parse(&r["pattern"]));
        if a.conforms_to(&b).unwrap() != r["refines"].as_bool().unwrap() {
            wrong.push(format!("{a} ⪯ {b}: model {}", r["refines"]));
        }
        if b.accepts(&a).unwrap() != r["refines"].as_bool().unwrap() {
            wrong.push(format!("{b} accepts {a}: model {}", r["refines"]));
        }
        if a.is_equivalent(&b).unwrap() != r["equivalent"].as_bool().unwrap() {
            wrong.push(format!("{a} ≡ {b}: model {}", r["equivalent"]));
        }
        if a.meets(&b).unwrap() != r["meets"].as_bool().unwrap() {
            wrong.push(format!("{a} meets {b}: model {}", r["meets"]));
        }
        if a.satisfies(&b).unwrap() != r["satisfies"].as_bool().unwrap() {
            wrong.push(format!("{a} satisfies {b}: model {}", r["satisfies"]));
        }
        if a.may_satisfy(&b).unwrap() != r["may_satisfy"].as_bool().unwrap() {
            wrong.push(format!("{a} may satisfy {b}: model {}", r["may_satisfy"]));
        }
    }
    let scores = table["scores"].as_array().unwrap();
    for r in scores {
        let u = parse(&r["urn"]);
        if u.specificity() as u64 != r["score"].as_u64().unwrap() {
            wrong.push(format!("specificity {u}: model {}", r["score"]));
        }
    }
    assert!(rows.len() > 4000 && scores.len() > 60, "the table is the full one");
    assert!(
        wrong.is_empty(),
        "{} answer(s) differ from the model, e.g.\n  {}",
        wrong.len(),
        wrong[..wrong.len().min(8)].join("\n  ")
    );
}
