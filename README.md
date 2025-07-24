# ospo-survey-analysis

This is my repo for analyzing the 2025 multi-campus UC open source survey.

It's very much a work in progress.

## Survey instrument

The survey instrument is available in the top level of this repo, and is called
"OSPO_survey_intrument.pdf". If you would like the instrument as a Qualtrics
file rather than a PDF, please email me at virginiascarlett@ucsb.edu.

## Data

The survey data are not in this repo (yet). I will release the de-identified
data eventually. Until then, the scripts in this repo are only runnable by me,
because only I have the data.

## Dependencies

I am using `renv` to manage my R packages. To see the R packages I'm using for
my analysis, go to `scripts/packages.R`. To add or remove packages, I edit
`scripts/packages.R`, and then run `renv::snapshot()` to edit my renv.lock file.
I also use quarto to render the markdown files, which is a CLI tool installed on
my local computer outside this repostiory.

## Notebooks

I conduct my analyses in quarto notebooks, which are executables that I
periodically render to PDF reports. These notebooks are like a lab notebook or a
journal--they show my stream of consciousness as I explore the data. They are
not concise summaries of my key results. I am publishing the rendered PDFs in
`reports/notebooks` (Quarto apparently insists on this spurious intermediate
directory).
