# Which Is Better?

An R Shiny app for comparing two review distributions.

The app is platform-agnostic and works from user-supplied evidence rather than live scraping. Each side can be entered as:

- a 1-to-5 histogram count table
- a raw ratings vector such as `c(5, 5, 4, 5)`
- a histogram screenshot that the app parses into editable 1-to-5 count suggestions

## Features

- frequentist difference-in-means test with Welch's t-test
- plain-language comparison summary with confidence interval and p-value
- short in-app generated report plus a fuller downloadable export in `.html`, `.docx`, `.md`, or `.tex`
- side-by-side star-count distribution plotting
- screenshot parser that estimates 1-to-5 counts and then lets the user edit them
- optional manual entry of displayed overall rating and total rating count to guide the parser when OCR is imperfect
- local SQLite history that saves labels, pasted URLs, and comparison metadata each time you analyze
- folded R code export so users can copy the reconstructed raw ratings into their own local R workflow, with compact `rep()`-based vectors

## Run locally

```r
source("scripts/install_deps.R")
shiny::runApp()
```

## Live deployment

GitHub Pages was removed because this app needs a live R runtime and a Python
screenshot parser. The repository is now configured for a Docker-based Shiny
deployment that works on Render.

Included deployment files:

- `Dockerfile`
- `render.yaml`
- `scripts/install_hosted_deps.R`
- `scripts/run_hosted.R`

### Render

1. Push this repository to GitHub.
2. Create a new Render Blueprint from the repository.
3. Let Render pick up `render.yaml`.
4. Deploy the web service.

Notes:

- `render.yaml` currently uses the `starter` plan because it attaches a
  persistent disk for the SQLite history database.
- The hosted build installs the screenshot parser dependencies in Python.
- The hosted build keeps the app lightweight and does not include the old Bayesian path.

## Notes

- Screenshot parsing is heuristic. The app estimates counts from the bar lengths and visible rating metadata, then lets the user confirm or edit the suggested counts.
- Local history is stored at `data/which_is_better.sqlite` by default.
- In container deployment, you can control the history location with the `WHICHISBETTER_DATA_DIR` environment variable.
