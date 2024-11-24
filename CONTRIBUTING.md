# Contributing to Echidna

First, thanks for your interest in contributing to Echidna! We welcome and
appreciate all contributions, including bug reports, feature suggestions,
tutorials/blog posts, and code improvements.

If you're unsure where to start, we recommend to join our [chat room](https://slack.empirehacking.nyc/)
(in the #ethereum channel) to discuss new ideas to improve this tool. You can also take a look at the [`help wanted`](https://github.com/crytic/echidna/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22)
issue labels.

## Bug reports and feature suggestions

Bug reports and feature suggestions can be submitted to our [issue
tracker](https://github.com/crytic/echidna/issues). For bug reports,
attaching the contract that caused the bug will help us in debugging and
resolving the issue quickly. If you find a security
vulnerability, do not open an issue; email `opensource@trailofbits.com`
instead.

## Questions

Questions can be submitted to the issue tracker, but you may get a faster
response if you ask in our [chat room](https://slack.empirehacking.nyc/)
(in the #ethereum channel).

## Code

Echidna uses the pull request contribution model. Please make an account on
Github, fork this repo, and submit code contributions via pull request. For
more documentation, look [here](https://guides.github.com/activities/forking/).

Some pull request guidelines:

- Minimize irrelevant changes (formatting, whitespace, etc) to code that would
  otherwise not be touched by this patch. Save formatting or style corrections
  for a separate pull request that does not make any semantic changes.
- When possible, large changes should be split up into smaller focused pull
  requests.
- Fill out the pull request description with a summary of what your patch does,
  key changes that have been made, and any further points of discussion, if
  applicable.
- Title your pull request with a brief description of what it's changing.
  "Fixes #123" is a good comment to add to the description, but makes for an
  unclear title on its own.
- We use a "CodeClimate" verification check in every PR to make sure the code merged 
  is tidy and easy to understand.
