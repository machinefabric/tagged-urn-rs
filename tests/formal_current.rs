//! TEST600: the checked-in generated code is what the model generates now.
//!
//! `src/generated` is committed so that a crate compiling this one needs
//! neither Lean nor lungo. The price is that it can fall behind `../formal`:
//! a model changed and not regenerated would leave this crate running the old
//! rules while its proofs described the new ones. This regenerates into a
//! scratch directory and compares, file by file.
//!
//! Ignored by default because it needs elan and cargo-lungo; the workspace's
//! test run passes `--ignored`. `scripts/generate-formal.sh` is the fix.

use std::path::{Path, PathBuf};
use std::process::Command;

fn files(root: &Path) -> Vec<PathBuf> {
    let mut found = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        for entry in std::fs::read_dir(&dir).expect("reading generated output") {
            let path = entry.expect("an entry").path();
            if path.is_dir() {
                stack.push(path);
            } else if path.file_name().and_then(|n| n.to_str()) != Some("build-info.json") {
                // build-info.json records machine paths and is not compiled.
                found.push(path.strip_prefix(root).unwrap().to_path_buf());
            }
        }
    }
    found.sort();
    found
}

#[test]
#[ignore = "needs elan and cargo-lungo; run with --ignored"]
fn test600_generated_code_is_current() {
    let crate_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let scratch = tempfile_dir();
    let status = Command::new("cargo")
        .args(["lungo", "build", "--out-dir"])
        .arg(&scratch)
        .current_dir(crate_dir)
        .status()
        .expect("running cargo lungo");
    assert!(status.success(), "cargo lungo build failed");

    let committed = crate_dir.join("src/generated/tagged_urn_formal");
    let fresh = scratch.join("tagged_urn_formal");
    let (a, b) = (files(&committed), files(&fresh));
    assert_eq!(a, b, "the generated file set differs; run scripts/generate-formal.sh");
    let stale: Vec<_> = a
        .iter()
        .filter(|f| std::fs::read(committed.join(f)).unwrap() != std::fs::read(fresh.join(f)).unwrap())
        .collect();
    assert!(
        stale.is_empty(),
        "src/generated is behind ../formal in {stale:?}; run scripts/generate-formal.sh"
    );
    let _ = std::fs::remove_dir_all(&scratch);
}

fn tempfile_dir() -> PathBuf {
    let dir = std::env::temp_dir().join(format!("tagged-urn-generated-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("a scratch directory");
    dir
}
