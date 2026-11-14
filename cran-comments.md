## Update

This is a small update that removes the NOTE currently shown on the R-devel
CRAN flavours: `structure()` calls used the deprecated special names `.Names`
and `.Dim` (`R/plotting-other.R`, `R/testing-helpers.R`, and two test files).
There are no other user-visible changes since 1.0.4. See `NEWS.md`.

## Test environments

* Ubuntu 24.04 (local), R 4.6.1: `R CMD check --as-cran`
* GitHub Actions (org reusable workflows): see the badge on the repository
* win-builder (release, devel), macOS builder and R-hub: TO DO before submission

## R CMD check results

0 errors | 0 warnings | 0 notes (local; the notes about the development
version number, `Remotes` and `.git` seen on the development branch go away when
the version is bumped and `Remotes` is removed for the release).

## Reverse dependencies

`SpaDES.core` (Imports) and `SpaDES.tools` (Suggests) use `quickPlot`.
`NetLogoR` will no longer depend on it after its own pending update. No exported
function changed, so none should be affected. `revdepcheck` TO DO before submission.
