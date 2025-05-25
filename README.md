# ospo-survey-analysis
This is my repo for analyzing the 2025 multi-campus UC open source survey.

It's very much a work in progress.

## Data

The survey data are not in this repo (yet). I will release the de-identified data eventually. Until then, the scripts in this repo are only runnable by me, because only I have the data.

## Dependencies
I am using `renv` to manage my R packages. To add or remove packages, I edit `scripts/packages.R`, and then run `renv::snapshot()` to edit my renv.lock file. I also use quarto to render the markdown files, which is a CLI tool installed on my local computer outside this repostiory.

## Publishing

I'm currently in the process of transitioning from R scripts to .qmd notebooks. I don't really love these... but the plan is to transition to them so that I can render my scripts to readable reports. As I go along, and some of the (.qmd) notebooks are basically finalized, I will publish them as pdfs in `reports/markdown` (Quarto insists on this spurious intermediate directory).

Note to self: to publish, do `quarto render --to pdf`.