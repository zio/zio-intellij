# scala-plugin-testkit

- **Source:** https://github.com/JetBrains/intellij-scala
- **Synced from commit:** `345dc52da7ec2e7427da901667549728d6c8e73d` (branch `idea262.release`)

Vendored copy of the IntelliJ **Scala plugin** test framework — the base classes, fixtures and helpers our
inspection/refactoring tests build on. JetBrains does not publish these as a Maven artifact, so they have to be copied
into the repository.

Almost every file is a verbatim copy, but a few carry unavoidable local adaptations, each tagged inline with
`// zio-local`. Do not "clean up" `// zio-local` lines when re-syncing — re-apply them.

Not all files are copied from the upstream, only those that are actually used by our tests.

This module exists purely so the vendored code is **physically isolated** from our own code to make it easier to re-sync
with upstream.

Run `./sync.sh` to copy the current upstream release branch and reapply our local changes as a patch.
The release branch is derived from GitHub's default branch by replacing the `.x` suffix with `.release`
(for example, `idea263.x` → `idea263.release`). Pass a branch to override this: `./sync.sh idea262.release`.
The copied upstream files are staged before the patch is applied. Use `git diff --cached`
to review that snapshot and `git diff` to review the reapplied local changes.

Excluded from scalafmt (see `.scalafmt.conf`) to avoid unrelated formatting differences from upstream.
