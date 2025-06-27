# ospo-survey-analysis

This is my repo for analyzing the 2025 multi-campus UC open source survey.

It's very much a work in progress.

## Data

The survey data are not in this repo (yet). I will release the de-identified
data eventually. Until then, the scripts in this repo are only runnable by me,
because only I have the data.

## Dependencies

I am using `renv` to manage my R packages. To add or remove packages, I edit
`scripts/packages.R`, and then run `renv::snapshot()` to edit my renv.lock file.
I also use quarto to render the markdown files, which is a CLI tool installed on
my local computer outside this repostiory.

## Publishing

I was originally doing my analysis with R scripts, but now that I am almost
exclusively working in quarto notebooks, which I periodically render to PDF
reports. I am publishing them in `reports/notebooks` (Quarto apparently insists
on this spurious intermediate directory). I am only publishing them as PDFs
here. I am not checking the html renders into version control because the web
files make my `git status` and git diffs very noisy. The PDF reports are long,
and not intended to be easy, approachable guides to the data. They are more like
a lab notebook. They represents my stream of consciousness as I explore the
dataset.
