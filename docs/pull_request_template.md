### Description

<!-- PR description goes here -->

### YT issue

https://issues.serokell.io/DSCP-

### Introduced changes

<!-- 
Check all that apply. At least one _should_ be checked, otherwise
describe that weird type of change in the description, maybe it will
make sense to add it to the list.
-->

- [ ] New feature
- [ ] Bugfix 
- [ ] Refactoring
- [ ] New tests (on functionality not fixed/introduced in this PR)
- [ ] New docs (on functionality not fixed/introduced in this PR)
- [ ] Infrastructure changes 

### Checklist

If I added new functionality, I
- [ ] added tests covering it
- [ ] explained it using haddock in the source code

If I fixed a bug, I
- [ ] added a regression test to prevent the bug from silently reappearing again

If I changed public API structure or/and data serialization, I made sure that
- [ ] those changes are reflected in [Swagger API specs](/specs/disciplina)
- [ ] and also in [related](/docs/api-types.md) [documentation](/docs/authentication.md).

If I changed config structure or/and CLI interface of any executable, I made sure that
- [ ] [sample config](/docs/config-full-sample.yaml) is updated accordingly
- [ ] [launch scripts](/scripts/launch) are updated accordingly and work as expected

If I changed the procedure of building a project and/or running any executable or helper script, I
- [ ] updated [README.md](/README.md) accordingly
- [ ] as well as subproject READMEs for all related executables

Also,
- [ ] my commit history is clean, descriptive and do not contain merge or revert commits or commits like "WIP"
- [ ] my code complies with the [style guide](/docs/code-style.md)
- [ ] my documentation is checked with a spell checker (like [Grammarly](https://app.grammarly.com))
