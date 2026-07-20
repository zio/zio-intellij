# scala-plugin-testkit

- **Source:** https://github.com/JetBrains/intellij-scala
- **Synced from commit:** `345dc52da7` (branch `idea262.release`)

Vendored copy of the IntelliJ **Scala plugin** test framework — the base classes, fixtures and helpers our
inspection/refactoring tests build on. JetBrains does not publish these as a Maven artifact, so they have to be copied
into the repository.

Almost every file is a verbatim copy, but a few carry unavoidable local adaptations, each tagged inline with
`// zio-local`. Do not "clean up" `// zio-local` lines when re-syncing — re-apply them.

Not all files are copied from the upstream, only those that are actually used by our tests.

This module exists purely so the vendored code is **physically isolated** from our own code to make it easier to re-sync
with upstream.

Excluded from scalafmt (see `.scalafmt.conf`) so the files stay byte-comparable with upstream.
