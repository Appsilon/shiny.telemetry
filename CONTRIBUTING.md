# Contributing
_*Have you read the [Appsilon Contributing Guidelines](https://github.com/Appsilon/.github/blob/main/CONTRIBUTING.md)?*_

## pre-commit
This project uses [pre-commit][] hooks. The hooks are a series of automated checks run locally.
These automate mundane tasks such as running spellchecking, linter, and other potential issues
before you even push your changes!

[pre-commit]: https://pre-commit.com

### Getting Started
To get started you need to install the pre-commit tool. Additionally, `{lintr}`, `{pkgdown}`, and
`{testthat}` packages have to be installed either globally or within `{renv}` (if your project uses
it). To install pre-commit, pick a method that suits you the best:

- using pip (package installer for Python): `pip install pre-commit` or `pip3 install pre-commit`
- using homebrew: `brew install pre-commit`
- using `{precommit}` package: follow [the installation section in its vignette][]

[the installation section in its vignette]: https://lorenzwalthert.github.io/precommit/articles/precommit.html#installation

Once you have installed pre-commit, run `pre-commit install` to set up the hooks.

### Modifying Package Dependencies
If package dependencies are being changed, then the list of `additional_dependencies` for
`roxygenize` hook has to be updated. The list can be generated using a helper function from
`{precommit}` package: `precommit::snippet_generate("additional-deps-roxygenize")`.

### Known Issues
- If you use MacOS and have installed R via homebrew, then chances are that pre-commit will fail to
  setup an environment. A workaround is installing R by other means, e.g. by using [rig][].
- `{lintr}` might report false positive warnings about `object_usage_linter`. This can be fixed by
  install the package with your contributions prior to running the linter.

[rig]: https://github.com/r-lib/rig
