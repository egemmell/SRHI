# Template for setting up reproducible projects

## About this Repository

This repository operationalises The Turing Way recommendations for structuring project repositories to support reproducibility in data science projects.
This repository includes files and directories recommended for enabling reproducibility and collaboration in a project, as well as sharing of research objects.

This repository follows the recommendations and guidance provided in *[The Turing Way](https://book.the-turing-way.org/)* handbook to data science.
When reusing this repository, please update the information on your README page with information about your project.

## About README

On an online repository, such as GitHub, the project overview page is named ‘README’ which is equivalent to the main page of a website.
README page should describe the project -- what is the purpose of the project, who is involved, how to collaborate and where to find key resources.

To learn more about how to create a README.md file, please read the [Landing Page - README File](https://book.the-turing-way.org/project-design/pd-overview/project-repo/project-repo-readme/) chapter in The Turing Way Guide for Project Design.

When reusing, you can delete most content written here, and use this MarkDown template to add content about your project


## Repo Structure

Inspired by [Cookie Cutter Data Science](https://github.com/drivendata/cookiecutter-data-science).

```
├── CHANGES.md           
├── DATAINFO.md          
├── README.md           <- The top-level README for users of this project.
├── TASKS.md             
│
├── assets             
│
├── data
│   ├── processed      <- The final, canonical data sets for modeling.
│   └── raw            <- The original, immutable data dump.
│
├── docs               <- A default Sphinx project; see sphinx-doc.org for details
│
├── notebooks          <- Jupyter notebooks. The naming convention is a number (for ordering),
│                         the creator's initials, and a short `-` delimited description, e.g.
│                         `1.0-jqp-initial-data-exploration`.
│
├── output             <- Generated analysis as HTML, PDF, LaTeX, etc.
│   └── figures        <- Generated graphics and figures to be used in reporting
│
├── src                <- Source code for use in this project.
│   │
│   ├── data           <- Scripts to download or generate data
│   │   └── name1.py
│   │
│   ├── analysis       <- Scripts to complete analysis
│   │   ├── name1.py
│   │   └── name2.py
│   │
│   └── visualisation  <- Scripts to create exploratory and results-oriented visualisations
│       └── name1.py
└──
```
---





<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->


