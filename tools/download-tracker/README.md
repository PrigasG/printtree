# Download Tracker

This is a maintainer-facing Shiny app for checking daily CRAN downloads
for `printtree`.

## Run locally

```r
shiny::runApp("tools/download-tracker")
```

## Data source

The app reads the public daily download feed from
`https://cranlogs.r-pkg.org`.

## Scheduled updates

The GitHub Actions workflow
`.github/workflows/download-stats.yaml` refreshes cached download data
weekly and stores it in `tools/download-tracker/data/`.
