---
trigger: always_on
---

# Rules

- Use commonly known abbreviations for long words.
  E.g. `idx` instead of `index`, `props` instead of `properties`, etc.
- Organize the file top-down, with leaf functions in the call graph lower than their callers.
  In other words, try to always put the function definition after all its usages.
- Prefer flat filesystem structure for source files. Avoid deeply nested directories.
- Prefer functions over classes.

## Testing

- Always run tests using `cargo test` after making changes.
- If snapshots changed, use `cargo insta accept` to update them.
- You can also run tests with `INSTA_UPDATE=always` to automatically update snapshots.
- Your job is done only after all tests pass.
