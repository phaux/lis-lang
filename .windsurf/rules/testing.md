---
trigger: always_on
---

- Always run tests using `cargo test` after making changes.
- If snapshots changed, use `cargo insta accept` to update them.
- You can also run tests with `INSTA_UPDATE=always` to automatically update snapshots.
- Your job is done only after all tests pass.
