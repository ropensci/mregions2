---
editor_options: 
  markdown: 
    wrap: sentence
---

# Contributing to mregions2

```{=html}
<!-- This CONTRIBUTING.md is adapted from:
https://gist.github.com/peterdesmet/e90a1b0dc17af6c12daf6e8b2f044e7c 
https://github.com/ropensci/skimr/blob/main/.github/CONTRIBUTING.md#understanding-the-scope-of-skimr
usethis::use_tidy_contributing()
-->
```
First of all, thanks for considering contributing to mregions2!
mregions2 is an open source project maintained by the data managers of [Marine Regions](marineregions.org/), who are quite busy updating maritime boundaries and curating the database and really appreciate any help.

## How you can contribute

There are several ways you can contribute to this project.
If you want to know more about why and how to contribute to open source projects like this one, see this [Open Source Guide](https://opensource.guide/how-to-contribute/).

### Share the love ❤️

Think mregions2 is useful?
Let others discover it, by telling them in person, via Twitter or a blog post.

Using mregions2 for a paper you are writing?
Consider [citing it](https://lifewatch.github.io/mregions2/authors.html).

### Ask a question ⁉️

Using mregions2 and got stuck?
Browse the [documentation](https://lifewatch.github.io/mregions2) to see if you can find a solution.
Still stuck?
Post your question as an [issue on GitHub](https://github.com/lifewatch/mregions2/issues/new).
While we cannot offer user support, we'll try to do our best to address it, as questions often lead to better documentation or the discovery of bugs.

Want to ask a question in private?
Contact Marine Regions by [email](mailto:info@marineregions.org).

### Propose an idea 💡

Have an idea for a new mregions2 feature?
Take a look at the [documentation](https://lifewatch.github.io/mregions2) and [issue list](https://github.com/lifewatch/mregions2/issues) to see if it isn't included or suggested yet.
If not, suggest your idea as an [issue on GitHub](https://github.com/lifewatch/mregions2/issues/new).
While we can't promise to implement your idea, it helps to: \* Explain in detail how it would work.
\* Keep the scope as narrow as possible.

See below if you want to contribute code for your idea as well.

### Report a bug 🐛

Using mregions2 and discovered a bug?
That's annoying!
Don't let others have the same experience and report it as an [issue on GitHub](https://github.com/lifewatch/mregions2/issues/new) so we can fix it.
A good bug report makes it easier for us to do so.
Please include:

-   Output of `sessionInfo()`

-   A minimal example made with [reprex](https://reprex.tidyverse.org/).

-   Any details about your local setup that might be helpful in troubleshooting.

### Improve the documentation 📖

Noticed a typo on the website?
Think a function could use a better example?
Good documentation makes all the difference, so your help to improve it is very welcome!

#### The website

[This website](https://lifewatch.github.io/mregions2) is generated with [`pkgdown`](http://pkgdown.r-lib.org/).
That means we don't have to write any html: content is pulled together from documentation in the code, vignettes, [Markdown](https://guides.github.com/features/mastering-markdown/) files, the package `DESCRIPTION` and `_pkgdown.yml` settings.
If you know your way around `pkgdown`, you can [propose a file change](https://help.github.com/articles/editing-files-in-another-user-s-repository/) to improve documentation.
If not, [report an issue](https://github.com/lifewatch/mregions2/issues/new) and we can point you in the right direction.

#### Function documentation

Functions are described as comments near their code and translated to documentation using [`roxygen2`](https://klutometis.github.io/roxygen/).
If you want to improve a function description: 1.
Go to `R/` directory in the [code repository](https://github.com/lifewatch/mregions2).
2.
Look for the function inside the files.
3.
[Propose a file change](https://help.github.com/articles/editing-files-in-another-user-s-repository/) to update the function documentation in the roxygen comments (starting with `#'`).

### Contribute code 📝

Care to fix bugs or implement new functionality for mregions2?
Awesome!
👏 Have a look at the [issue list](https://github.com/lifewatch/mregions2/issues) and leave a comment on the things you want to work on.
See also the development guidelines below.

## Development guidelines

We try to follow the [GitHub flow](https://guides.github.com/introduction/flow/) for development.

1.  Fork [this repo][repo] and clone it to your computer. To learn more about this process, see [this guide](https://guides.github.com/activities/forking/).
2.  If you have forked and cloned the project before and it has been a while since you worked on it, [pull changes from the original repo](https://help.github.com/articles/merging-an-upstream-repository-into-your-fork/) to your clone by using `git pull upstream main`.
3.  Open the RStudio project file (`.Rproj`).
4.  Make your changes:
    -   Write your code.
    -   Write unit tests (see testing documentation below).
    -   Document your code (see function documentation above).
    -   Check your code with `devtools::check()` and aim for 0 errors and warnings.
5.  Commit and push your changes.
6.  Submit a [pull request](https://guides.github.com/activities/forking/#making-a-pull-request).

### Style

We follow the [tidyverse style guide](http://style.tidyverse.org/) for new code.

To any new commits, please abide by the [Conventional Commits specification](https://www.conventionalcommits.org/).

![](https://i.imgflip.com/7yfy4f.jpg){width="350"}

### Testing

Please try to provide 100% test coverage for any submitted code and always check that existing tests continue to pass.
If you are a beginner and need help with writing a test, mention this in the issue and we will try to help.

We use [testthat](https://cran.r-project.org/package=testthat) for unit tests and [httptest2](https://cloud.r-project.org/package=httptest2) to mock-up HTTP requests.
New contributions that affect any functionality regarding HTTP requests must be properly mocked up using [httptest2](https://cloud.r-project.org/package=httptest2).
In addition, we also add the same test without mocking to `./real-tests/` to check if the API response is still the same.
We follow the recommendations of the book [HTTP testing in R](https://books.ropensci.org/http-testing/index.html).

## Code of Conduct

Please note that the mregions2 project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By contributing to this project you agree to abide by its terms.

