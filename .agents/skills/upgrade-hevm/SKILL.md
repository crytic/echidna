---
name: upgrade-hevm
description: Upgrade Echidna's hevm dependency to upstream main or a requested revision, refresh the Nix hash and Stack lockfile, fix compatibility issues, and commit the tested upgrade on a local branch.
---

# Upgrade hevm

Run from the Echidna repository root.

## Select the target and worktree

1. Default to `main` from `argotorg/hevm`. Honor a requested fork, branch, tag, or commit. If the user refers to another target without identifying it, ask which one. Resolve the ref once with `sha=$(gh api "repos/$repo/commits/$ref" --jq .sha)`, where `repo` is the selected `owner/name`. Use the full SHA throughout.
2. Check `git status`, `git worktree list`, and any ignored `cabal.project.local`. If local edits or a sibling hevm override would affect verification, use a clean linked worktree from the intended base. Honor the requested branch. For a new topic branch, use an available flat name such as `upgrade-hevm-<short-sha>`. This works alongside an existing `upgrade-hevm` branch.
3. Record the old `fetchFromGitHub` revision as `old_sha`. Compare the target **repository and revision** with the hevm block in `flake.nix`, the Git dependency in `stack.yaml`, the `original` and `completed` entries in `stack.yaml.lock`, and the Windows patch URL in `.github/scripts/install-libff.sh`. Use `git grep -n -F "$old_sha"` to locate additional tracked references. If everything already matches, keep the real hash and lockfile and go straight to verification. If only some pins have drifted, repair those entries.

## Refresh the pins

Set the flake's hevm owner, repo, and revision and Stack's hevm Git URL and commit to the target. Once those inputs are set, the following checks can run in parallel. Finish the pin edits before building the updated package.

- **Nix source.** When the flake source changes, set its `sha256` to `pkgs.lib.fakeHash` and run `nix build --no-link .#echidna`. Copy the `got:` SRI hash from the hevm fixed-output mismatch into `sha256`. If the fetch fails for a different reason, resolve it and retry. This direct fetch requires no `flake.lock` update.
- **Stack lockfile.** Run `stack --lock-file read-write config build-files`, or `nix shell nixpkgs#stack -c stack --lock-file read-write config build-files` when Stack is absent. Verify both hevm URLs and commits against `stack.yaml`, keep Stack's generated version and Pantry tree hash and size, and investigate unrelated changes. [Stack lockfile update procedure](https://docs.haskellstack.org/en/stable/topics/lock_files/#update-procedure)
- **Windows patch.** Verify that `.github/scripts/libff.patch` exists at the pinned hevm commit before updating the URL in `.github/scripts/install-libff.sh`. If upstream moved or retired it, inspect how that revision builds libff on Windows before adapting the script.

When the hevm revision changes, review the upstream changes from `old_sha` to `sha` and update the top `## Unreleased` section of `CHANGELOG.md`. Write one short sentence using the first eight characters of the new SHA in backticks.

```markdown
* Updated hevm to `<short-sha>` with <highlights relevant to Echidna users>
```

If this release cycle already has a hevm update, revise that bullet in place. Fold in earlier hevm update bullets from the same section, keep the most important highlights that still apply across the whole cycle, and use the newest hash. Otherwise, add the bullet at the top of the section.

## Build and test

With the real source hash in place, enter `nix develop` in the verification worktree. The shell runs Hpack and provides the pinned hevm library. Use Cabal's incremental build for compatibility fixes, compiling the same components and treating warnings as errors on each pass.

```sh
cabal build --constraint='hevm installed' --ghc-options=-Werror \
  lib:echidna exe:echidna test:echidna-testsuite
```

Check changed APIs in the pinned hevm source. Preserve caller behavior when migrating deprecated calls or handling new constructors. For a new record field, inspect how hevm initializes and uses it before updating Echidna's constructors. Keep the existing warning flags enabled. For dependency changes, edit `package.yaml` and the relevant build pins, then refresh the Stack lock as needed.

1. **Choose the main test compiler.** Inspect the guards with `rg -n 'solcV|withSolcVersion|testForgeStd|solcSupportsForgeStd' src/test`. Prefer a runnable compiler that covers the most cases in one full suite. For example, solc 0.8.25 covers both forge-std and transient storage. Probe each candidate inside the test shell and verify the reported version. The Nix shell prepends its own solc. The following selects an installed solc-select version for this shell.

   ```sh
   export PATH="$(dirname "$(command -v solc-select)"):$PATH"
   export SOLC_VERSION=0.8.25
   solc --version
   ```

2. **Prepare the Foundry fixture.** When the compiler satisfies the forge-std guard, initialize the pinned submodule in the verification worktree with `git submodule update --init -- tests/solidity/foundry/forge-std`.
3. **Run the full suite from the worktree root** with the selected compiler.

   ```sh
   cabal test --constraint='hevm installed' --ghc-options=-Werror \
     --test-show-details=streaming echidna-testsuite
   ```

4. **Account for skipped bodies.** Both `withSolcVersion` in `src/test/Common.hs` and `testForgeStd` in `src/test/Tests/FoundryTestGen.hs` return a passing test when their compiler guard fails. Match the guards and their registered cases to the compiler actually used. For each other runnable compiler, rerun just the cases it can cover by adding Tasty filters to the same Cabal command, for example `--test-option=-p --test-option=fullmath` with solc 0.7.5. Check that the filter selected the intended cases. Count each executed case once across runs and report any cases still skipped and why.

After the final compatibility fix and pin edits, verify the package with `nix build --no-link .#echidna`. If another fix is needed, repeat Cabal verification and the final package build.

## Review and commit

Stage the pins, generated lockfile, changelog entry, and required compatibility changes by path. Review `git diff --cached` and run `git diff --cached --check`. If it flags space indentation, compare the lines with `HEAD` and inspect `git config --show-origin --get core.whitespace`. When a global `indent-with-non-tab` rule flags existing indentation, preserve the file's style and rerun the check with that rule disabled for this command. Fix any new whitespace errors.

```sh
git -c core.whitespace="$(git config --get core.whitespace),-indent-with-non-tab" diff --cached --check
```

Commit locally with a subject such as `Update hevm to <short SHA>`. If verification produced no changes, report that the pin is current and skip the empty commit. Report the branch, commit, old and new hevm revisions, build results, and test coverage by compiler.
