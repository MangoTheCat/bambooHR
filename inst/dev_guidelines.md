# Development guidelines

The following points are here for the developers to make sure they are following consistent standards.

## Development workflow
1. Create a feature or bugfix branch (see [branching](#branching))
2. Development (see details in [function writing](#function-writing))
3. Write function-specific unit tests(`devtools::use_test('test_name')`
4. Document the changes (`devtools::document()` or **Ctrl+Shift+D**)
5. Check all unit tests pass in the package (`devtools::test()`)
6. Check if there are any other issues in the package by running a R CMD check (`devtools::check()`)
7. Make sure the lock file for {renv} is up to date if you added any new packages/dependencies.
8. Push and create a pull request (PR) from your feature branch in to the develop branch
9. If there are any merge conflicts, merge the develop branch in to your feature branch and solve the conflicts in the feature branch (see [Resolving git conflicts](#resolving-git-conflicts)

## Branching

We will have a `main` branch, and a `develop` branch. When tackling an issue which has a corresponding issue/ticket, create a branch`ticketY` from `develop`, where `Y` is the ticket number/ID (can optionally extend the branch name with a succinct description of the work required e.g. `ticketY-fix-bug-x`). Once the work is complete and tested, create a pull request for the branch `ticketY` in to `develop`.

When we are ready to make a new release, we will merge `develop` in to `main`.

## Package building
* We use `devtools`, `usethis`, `testthat` and `renv` packages for package development.
* We namespace calls to functions from other packages, e.g. `dplyr::filter()`, and specify a dependency on that package in the DESCRIPTION file (`usethis::use_package('package_name')`)
* If a function is used very often in a package (e.g. the magrtittr pipe), we can use a central script with roxygen's `@importFrom` to simplify things.

See [R packages by Hadley Wickham](https://r-pkgs.org/) for reference.

## Function Writing
* Create a new .R script in `/R`: `usethis::use_r('script_name')
* Add the roxygen skeletion: *Ctrl+Alt+Shift+R*.
* Update the title.
* **Always** document the argument type, as well as its description e.g. `@param x {numeric} a number of iterations`.
* Define the expected output using `@return`, e.g. `@return A numeric vector of length 1`.
* Export all the functions relevant to package users using the @export (can be skipped for internal/utils functions).
* Where possible, provide examples (in `@examples`).
* You can interactively test your code by mocking attaching the package with `devtools::load_all()`.

## Dependency Management with {renv}
* Reference [renv vignette](https://rstudio.github.io/renv/articles/renv.html)
* Ensure you're working with the correct version of R itself: You can look up the version in the `renv.lock` file which you can open in regualr editor or RStudio. You can change the version of R in the topright corner of RStudio Server.
* `renv::init()`: Initializes the current working directory as a renv project. Run it when starting a new R Project.
* `renv::snapshot()`: Save the current state of your library. Run after installing a new package.
* `renv::restore()`: Restore the library state saved in the most recent snapshot. Run after pulling a branch when you want to restore the latest library state.

## Merge review workflow
1. Pull the branch to be reviewed (in Shell: `git checkout origin/branch_to_review`).
2. Understand the scope of the feature and expected outputs.
3. Try to break the developed functions by running it through different examples, unexpected/wrong inputs etc.
4. Check if the function descriptions are clear and concise.
5. Check if the newly developed functions have corresponding unit tests.
6. Update the package documentation (`devtools::document()`)
7. Check all unit tests pass (`devtools::test()`)
8. Check if there are any other issues in the package (`devtools::check()`)
9. If there are any major issues, request changes to the PR.
10. When all the points have passed, approve the PR and delete the feature branch.

## Release workflow (to be expanded to include CRAN steps)
1. Pull latest `develop` branch
2. Make sure documentation, tests and R CMD check all look good.
3. Bump up package version in DESCRIPTION.
4. Write achievements in NEWS.md
5. Merge `develop` branch in to `main`.

## Git
### Resolving git conflicts
1. Make sure all your branches are up-to-date (by running `git fetch --all` and `git pull`: the latter needs to be run for each of the branches of interest).
2. Navigate to the branch you want to work on (e.g. `git checkout feature-branch-name`).
3. Make a merge (e.g. `git merge develop`).
4. If there are any conflicts, those will be listed in the Terminal/Shell and additionally marked orange in RStudio's git screen. a) If you want
to cancel the merge, run `git merge --abort`. b) If you're happy to continue the merge, read on.
5. Navigate to the conflicted files and edit them so that they contain the desired code version.
6. Once finished, stage the changed files (`git add .` or `git add file-name1 file-name2`).
7. Confirm that all relevant files have been staged (`git status` - all staged files will appear green).
8. Commit the changes (`git commit -m 'commit message'`).
9. Your merge has been completed. To view the commit history (with commit numbers) run `git log` (press `q` to return to the command line screen).

### Undoing the changes
1. If you want to move HEAD of the branch to one of the previous commits (e.g. undoing the merge) run `git reset --hard <commit-hash-to-move-to>`. This change will remove any changes/commits that were added after the specified commit.
2. If you want to remove the merge while keeping the merge commit in the commit history, run `git revert -m 1 <merge commit hash>`.

