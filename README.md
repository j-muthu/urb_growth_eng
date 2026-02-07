# UK Urban Growth Counterfactual Analysis

An interactive Shiny app that estimates the economic effects of relaxing planning regulations in English cities, applying the spatial equilibrium framework of [Duranton & Puga (2023)](https://onlinelibrary.wiley.com/doi/full/10.3982/ECTA17936) to UK data.

Users can adjust model parameters, select which cities to deregulate, and explore how counterfactual permitting rates affect national income, city-level income, and consumption for both incumbents and newcomers.

## How it works

The model calibrates a spatial equilibrium for English cities using 2001 and 2021 Built-Up Area (BUA) population data. For a given set of cities and a target permitting rate, it computes counterfactual populations, finds the new marginal city, and calculates welfare changes relative to the baseline.

Key parameters (with UK-adapted central estimates and ranges):

| Parameter | Description | Central | Range |
|-----------|-------------|---------|-------|
| &gamma; | Elasticity of commuting cost w.r.t. distance | 0.05 | 0.03 -- 0.08 |
| &theta; | Population elasticity of congestion (travel speed) | 0.06 | 0.05 -- 0.07 |
| &sigma; | Short-run agglomeration elasticity | 0.025 | 0.008 -- 0.038 |
| &beta; | Learning/experience agglomeration elasticity | 0.015 | 0.01 -- 0.03 |
| &lambda; | Land share in rural production | 0.12 | 0.10 -- 0.16 |

Reference permitting rates (75th/95th percentile UK cities, UK max, and Austin TX) are shown as dashed vertical lines on each chart.

## Repository structure

```
urb_growth_eng/
├── code/
│   ├── app.R                          # Entrypoint for deployment
│   ├── app/
│   │   ├── global.R                   # Data loading, city sets, reference rates
│   │   ├── ui.R                       # Shiny UI layout
│   │   └── server.R                   # Reactive logic and chart rendering
│   ├── counterfactual_functions.R     # Core model and chart functions
│   ├── agg_effs_urb_growth_uk.R       # Standalone batch analysis script
│   ├── parameter_estimates.csv        # Parameter ranges and sources
│   ├── Data/                          # Input datasets (BUA populations, permits, etc.)
│   ├── Outputs/                       # Generated CSVs from batch runs
│   └── manifest.json                  # R dependency manifest for deployment
├── latex/                             # Write-up source files
└── stata/                             # Supplementary Stata analysis
```

## Running locally

**Prerequisites:** R (>= 4.1) with the following packages:

```r
install.packages(c("shiny", "tidyverse", "readr", "plotly", "scales"))
```

**Launch the app** from the repository root:

```r
shiny::runApp("code")
```

Or from within the `code/` directory:

```r
shiny::runApp("app")
```

## Deploying

### Posit Connect Cloud (recommended)

1. Push the repo to GitHub
2. Go to [connect.posit.cloud](https://connect.posit.cloud) and link your GitHub account
3. Select the repo and set the content directory to `code/`
4. It will auto-deploy on push to `main`

### shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<ACCOUNT>", token = "<TOKEN>", secret = "<SECRET>")
rsconnect::deployApp(appDir = "code")
```

Get your token and secret from shinyapps.io under Account > Tokens.

## Data sources

- **City populations:** ONS Built-Up Area (BUA) estimates for 2001, 2011, and 2021
- **Geographic constraints:** Local Authority District-level geographic constraint variables
- **Austin permitting data:** City of Austin issued construction permits (for US reference rate)
- **Parameter estimates:** UK adaptations of Duranton & Puga (2023), Graham & Melo (2011), D'Costa & Overman (2014), and others (see `code/parameter_estimates.csv` for full sourcing)
