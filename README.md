# ospo-survey-analysis
This is my repo for analyzing the 2025 multi-campus UC open source survey.

It's very much a work in progress.

## Data
The survey data are not in this repo (yet). I will release the de-identified data eventually. Until then, the scripts in this repo are only runnable by me, because only I have the data.

## Dependencies
I am using `renv` to manage my R packages. To add or remove packages, I edit `scripts/packages.R`, and then run `renv::snapshot()` to edit my renv.lock file. I also use quarto to render the markdown files, which is a CLI tool installed on my local computer outside this repostiory. There are also some files here related to my formatting workflow, which are not necessary to run the executables: .lintr and air.toml.

## Publishing
I'm currently in the process of transitioning from R scripts to .qmd notebooks. The plan is to eventually phase out the scripts, so that I can solely work in notebooks and periodically render my notebooks to readable reports. I will publish them in `reports/notebooks` (Quarto apparently insists on this spurious intermediate directory). I will render the PDFs more frequently than the web files, because the web files make my `git status` and git diffs noisy. So the pdfs will always be up-to-date, but the web files might not be.