---
trigger: always_on
---

# Rules

- Use commonly known abbreviations for long words.
  E.g. `idx` instead of `index`, `props` instead of `properties`, etc.
- Organize the file top-down, with leaf functions in the call graph lower than their callers.
  In other words, try to always put the function definition after all its usages.
- Prefer flat filesystem structure for source files. Avoid deeply nested directories.

## Workflow

- Run tests with `INSTA_UPDATE=always cargo test` after making changes.
- Check code with `cargo clippy` before finishing work.
- Your job is done only after all tests pass.
- Format code with `cargo fmt` after you're done.
