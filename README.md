# ospo-survey-analysis

## Analysis Workflows for the UC OSPO Network Survey

![Project Status: Completed](https://img.shields.io/badge/status-completed-blue)

This repository contains the complete analysis workflows for the paper "A
System-wide Snapshot: A Multi-Campus Survey of Open Source Contributors at the
University of California". We may make little tweaks if changes are requested,
but this repository is no longer under active development. For the final
"snapshot" of this repository at time of publication, see DRYAD LINK (COMING
SOON).

## 📄 Citation

Please cite the resulting paper if you use this code or data:

COMING SOON!

## Background

The University of California (UC) OSPO Network is working to develop
infrastructure for open source education, discovery, and sustainability at UC.
To develop our strategic priorities and assess the state of UC open source, we
conducted a survey in April 2025 of 294 UC-affiliated open source contributors.
This repository contains the full analysis workflow used to clean, explore, and
visualize the survey data, which sheds light on how and why academics contribute
to open source and the barriers they face.

## 📊 Data and Survey Instrument

- **Survey Instrument:** The full survey instrument is available in the root of
  this repository as **`OSPO_survey_instrument.pdf`**. If you require the
  original Qualtrics file, please contact `virginiascarlett@ucsb.edu`.
- **Data:** The de-identified survey data used for this analysis is not stored
  in this repository. It is permanently archived at:
  > COMING SOON

## Reproducing the Analysis

### Download the Data and Configure Paths

To run the analysis, please download the data from Dryad and place it wherever
you like (the `data/` directory would be a reasonable location).

Note that the following data files are not in Dryad: intermediate files that can
be regenerated from this code, files containing personally identifiable
information, or files containing the same information that is already in the
supplmentary materials of the paper.

You will need to hard-code two paths for these scripts to work: DATA_PATH and
FIGURE_PATH. The scripts will look for "raw" data in the DATA_PATH location, and
will deposit final data in this location. They will deposit figures in the
FIGURE_PATH location. The way it's set up now, the paths should be in your
.Renviron file. \
For example:

```
DATA_PATH = "/Users/virginiascarlett/foo/bar/data/"
FIGURE_PATH = "/Users/virginiascarlett/foo/bar/figures"
```

If using the .Renviron file is not convenient for you, you can just hard code
the paths at the top of my utils.R script.

### Dependencies

This project uses **`renv`** to manage R package dependencies. To reproduce this
environment:

1.  Clone this repository to your local machine.
2.  Open the **`ospo-survey-analysis.Rproj`** file in RStudio or Positron.
3.  Run the following command in the R console to restore the exact package
    versions used in the analysis:
    ```r
    renv::restore()
    ```

The analysis notebooks are written as Quarto (`.qmd`) files. To render the
reports, you must install the [Quarto CLI](https://quarto.org/docs/get-started/)
on your computer.

### Running the Analysis

The primary analysis is contained in the Quarto notebooks in the `/notebooks`
directory. You can run the code chunks interactively or render+execute the
entire notebook with e.g. `quarto render my_notebook.qmd`.

The first notebooks that were run are data_cleanup_part1.qmd and
data_cleanup_part2.qmd. You won't be able to run these since they require the
raw data from Qualtrics. The data in Dryad are outputs from
data_cleanup_part2.qmd.

You can run most of the analysis notebooks in any order. Where one notebook
depends on the outputs of another, I have tried to include that information in
the introduction at the top of the notebook. The Quarto notebooks in /notebooks
are intended as an "analysis journal." They reflect the exploratory,
"stream-of-consciousness" process of data analysis, not a final, concise summary
of key results. The rendered PDF versions are available in /reports/notebooks.

The figure scripts are just for producing figures. They do not contain any
analysis. They draw on the final parsed data in a folder called
"data_for_plots".

## Repository structure

```
├── data/ # Recommended: put Dryad data here and set this as your DATA_PATH \
├── notebooks/ # Core analysis scripts (.qmd) \
│ └── defunct/ # Old junk \
├── reports/ \
│ └── notebooks/ # Rendered PDF and HTML reports \
├── renv/ # R environment files \
├── scripts/ \
│ ├── utils.R # Utility functions used in notebooks \
│ ├── packages.R # A list of all library() calls for renv to pick up \
│ ├── Other one-off data cleaning scripts \
│ └── defunct/ # Old junk \
├── .Rprofile \
├── .gitignore \
├── LICENSE \
├── OSPO_survey_instrument.pdf # The survey instrument \
├── README.md # This file \
├── \_quarto.yml # Quarto project configuration \
├── lessons_learned.md # Project retrospective \
├── ospo-survey-analysis.Rproj # RStudio Project file \
└── renv.lock # R environment lock file \
```
