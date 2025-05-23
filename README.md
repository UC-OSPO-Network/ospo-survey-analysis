# ospo-survey-analysis
This is my repo for analyzing the 2025 multi-campus UC open source survey.

It's very much a work in progress.

## Data

The survey data are not in this repo (yet). I will release the de-identified data eventually. Until then, the scripts in this repo are only runnable by me, because only I have the data.

## Package management
I am using `renv` to manage package versions. I just load all my packages for this project at the top of each script by sourcing the `scripts/packages.R` file. To add or remove packages, I edit `scripts/packages.R`, and then run `renv::snapshot()` to edit my renv.lock file. I also use quarto to render the markdown files, which is a CLI tool installed on my local computer outside this repostiory.

## Publishing

I'm doing my rapid, iterative data analysis in plain old R scripts, which are in the `scripts` directory. As I go along, and some of the scripts are basically finalized, I will manually convert them to .qmd documents with more prose in the `markdown` directory. (If you're wondering why I'm not coding in notebooks, it's because I don't like notebooks as an interface/IDE, but I like the nice reports you get from rendering notebooks to html. I also prefer .qmd documents over .ipynb files because the git diffs are cleaner.) My current plan is to produce the final figures from the notebooks, so those will be our 'source of truth' in the end.

So if you want to see the latest code, check out `scripts`. If you want to see the pretty HTML reports, go to `reports/markdown`. (Quarto insists on this spurious intermediate directory.)
