# ospo-survey-analysis

## Abstract

In the technology industry, the Open Source Program Office (OSPO) is a common
way to centralize a company’s open source strategy, knowledge, and diligence.
Meanwhile, OSPOs are just beginning to take root in academia. While guidance for
university OSPOs is emerging, many questions remain about how OSPOs best support
their university’s strategic priorities and their community’s needs.

The [University of California (UC) OSPO Network](https://ucospo.net) is working
to develop infrastructure for open source education, discovery, and
sustainability at UC by pooling our resources and knowledge. To develop our
strategic priorities and to assess the state of UC open source, we conducted a
survey in April 2025 of more than 230 UC-affiliated open source contributors.
This survey sheds light on how and why academics contribute to open source
projects, as well as some of the barriers holding them back.

We anticipate releasing a preprint in November 2025. Stay tuned!

## Survey instrument

The survey instrument is available in the top level of this repo, and is called
"OSPO_survey_intrument.pdf". If you would like the instrument as a Qualtrics
file rather than a PDF, please email me at virginiascarlett@ucsb.edu.

## Data

The survey data are not in this repo (yet). I will release the de-identified
data shortly before releasing the preprint. Until then, the scripts in this repo
are only runnable by me, because only I have the data.

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
