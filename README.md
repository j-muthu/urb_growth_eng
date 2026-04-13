# UK Urban Growth Counterfactual Analysis

An interactive app that estimates the economic effects of relaxing planning regulations in English cities. It applies the spatial equilibrium framework of [Duranton & Puga (2023)](https://onlinelibrary.wiley.com/doi/full/10.3982/ECTA17936) to UK data.

Users can adjust model parameters, select which cities to deregulate, and explore how counterfactual permitting rates affect national income, city-level income, and consumption for both incumbents and newcomers.

## Deployments

| Deployment | Stack | URL |
|------------|-------|-----|
| **Python (primary)** | Vanilla HTML/JS + Python serverless API on Vercel | <https://urban-growth-england.vercel.app/> |
| R (legacy) | R Shiny on Posit Connect Cloud | <https://joshua-muthu-urban-growth.share.connect.posit.cloud> |

Both deployments produce identical results — the Python backend is a direct port of the R computation and has been validated to match R output to 6+ decimal places.

## How it works

The model calibrates a spatial equilibrium for English cities using 2001 and 2021 Built-Up Area (BUA) population data. For a given set of cities and a target permitting rate, it computes counterfactual populations, finds the new marginal city, and calculates welfare changes relative to the baseline.

Key parameters (with UK-adapted central estimates and ranges):

| Parameter | Description | Central | Range |
|-----------|-------------|---------|-------|
| &gamma; | Elasticity of commuting cost w.r.t. distance | 0.05 | 0.03 -- 0.08 |
| &theta; | Population elasticity of congestion (travel speed) | 0.07 | 0.05 -- 0.08 |
| &sigma; | Short-run agglomeration elasticity | 0.025 | 0.008 -- 0.0538 |
| &beta; | Learning/experience agglomeration elasticity | 0.025 | 0.01 -- 0.04 |
| &lambda; | Land share in rural production | 0.072 | 0.05 -- 0.10 |

Reference permitting rates (75th/95th percentile UK cities, UK max, and Austin TX) are shown as dashed vertical lines on each chart.

## Repository structure

```
urb_growth_eng/
├── python/                              # Python app (primary deployment)
│   ├── api/
│   │   └── compute.py                   # Vercel serverless endpoint (POST)
│   ├── lib/
│   │   ├── constants.py                 # Population, GDP, city set definitions
│   │   ├── data.py                      # CSV loading, rate sequence, pop totals
│   │   └── model.py                     # Core computation (port of R)
│   ├── public/
│   │   ├── index.html                   # Single-page app UI
│   │   ├── app.js                       # Plotly.js chart rendering & interactivity
│   │   └── style.css                    # Layout & styling
│   ├── data/                            # Input CSVs (city data, params, reference rates)
│   ├── vercel.json                      # Vercel routing & build config
│   ├── pyproject.toml                   # Python project config
│   └── requirements.txt                 # numpy, pandas
│
├── R/                                   # R Shiny app (legacy deployment)
│   ├── app.R                            # Entrypoint for deployment
│   ├── app/
│   │   ├── global.R                     # Data loading, city sets, reference rates
│   │   ├── ui.R                         # Shiny UI layout
│   │   └── server.R                     # Reactive logic and chart rendering
│   ├── counterfactual_functions.R       # Core model and chart functions
│   ├── agg_effs_urb_growth_uk.R         # Standalone batch analysis script
│   ├── parameter_estimates.csv          # Parameter ranges and sources
│   ├── Data/                            # Input datasets (BUA populations, permits, etc.)
│   ├── Outputs/                         # Generated CSVs and PNGs from batch runs
│   └── manifest.json                    # R dependency manifest for deployment
│
├── latex/                               # Write-up (LaTeX source, bibliography, figures)
├── dp_original_stata_code/              # Reference Stata code from Duranton & Puga
└── wrluri/                              # Wharton Residential Land Use Regulatory Index data
```

## Running locally

### Python (recommended)

**Prerequisites:** Node.js (for Vercel CLI), Python >= 3.9

```bash
# Install the Vercel CLI (once)
npm install -g vercel

# Install Python dependencies
cd python
pip install -r requirements.txt

# Start the local dev server
vercel dev
```

The app will be available at `http://localhost:3000`.

### R

**Prerequisites:** R (>= 4.1) with the following packages:

```r
install.packages(c("shiny", "tidyverse", "readr", "plotly", "scales"))
```

**Launch the app** from the repository root:

```r
shiny::runApp("R")
```

Or from within the `R/` directory:

```r
shiny::runApp("app")
```

## Deploying

### Python on Vercel (primary)

1. Install the Vercel CLI: `npm install -g vercel`
2. From the `python/` directory, run `vercel` and follow the prompts to link your Vercel account
3. To deploy to production: `cd python && vercel --prod`
4. Subsequent pushes can be auto-deployed by linking the GitHub repo in the Vercel dashboard (set the root directory to `python/`)

### R on Posit Connect Cloud (legacy)

1. Push the repo to GitHub
2. Go to [connect.posit.cloud](https://connect.posit.cloud) and link your GitHub account
3. Select the repo and set the content directory to `R/`
4. It will auto-deploy on push to `main`

## Data sources

- **City populations:** ONS Built-Up Area (BUA) estimates for 2001, 2011, and 2021
- **Geographic constraints:** Local Authority District-level geographic constraint variables
- **Austin permitting data:** City of Austin issued construction permits (for US reference rate)
- **Parameter estimates:** See `R/parameter_estimates.csv` and the accompanying blog post
