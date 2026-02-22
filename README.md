# RMSTpowerBoost: Interactive Power and Sample Size Calculator  [![codecov](https://codecov.io/github/UTHSC-Zhang/RMSTpowerBoost-App/graph/badge.svg?token=RLAPUB8Q6I)](https://codecov.io/github/UTHSC-Zhang/RMSTpowerBoost-App)

This repository documents and assembles the RMSTpowerBoost Shiny application as an engineering system.  
This README is for contributors and maintainers: architecture, module boundaries, data flow, and quality wiring.

-----

## Purpose and Audience

- Audience: maintainers extending modeling methods, simulation logic, reporting, and QA/CI integration.
- Scope: internal system structure and component interactions.
- Out of scope: end-user operation and launch walkthroughs.

-----

## Repository Architecture

- `app.R`: single-file Shiny shell, UI layout, server orchestration, validation, report hooks, and integration with method engines.
- `R/`: modeling and simulation engines (analytical + bootstrap variants, diagnostics, recipe simulator, recipe set generation/loading).
- `data/`: packaged datasets consumed by app methods and tests.
- `data-raw/`: source material and scripts used to regenerate bundled data assets.
- `tests/testthat/`: coverage harness and targeted simulation/branch correctness tests.
- `.github/workflows/`: CI automation for coverage upload and package sync.
- `coverage/`: generated coverage artifacts tracked for upload and diagnostics.
- `tools/`: local engineering utilities (for example, coverage artifact generation/upload helper).
- `codecov.yml`: Codecov policy (thresholds, precision, rounding, ignore patterns).
- `DESCRIPTION` and `NAMESPACE`: package metadata/imports and exported function surface.

-----

## Application Composition (`app.R`)

`app.R` is a coordinator layer around method modules in `R/`:

- Package loading guard: validates all required UI/report/data packages before app object creation.
- Repo-root guard: verifies execution context is the repository root/workspace and resolves app-relative paths.
- Dynamic module loading: sources all `R/*.R` files at startup so method engines are available to the server layer.
- Helper/validation layer: coercion and schema guards for time/status/arm inputs, formatting helpers, plotting adapters, and UI reset helpers.
- UI assembly: central fluid-page layout with method selection, data source controls, diagnostics, plots, tables, and report export controls.
- Server orchestration: state management (`reactiveValues`), input/event handlers, method dispatch, diagnostics execution, and rendering.
- Report generation hooks: inline report template construction and HTML/PDF export paths integrated with computed results.

Design note: `app.R` intentionally keeps orchestration in one file and delegates statistical computation/simulation to dedicated files under `R/`.

-----

## Core Engine Modules (`R/`)

### Method families

- `R/linear_ipcw_analytical_app.R` and `R/linear_ipcw_boot_app.R`
  - linear IPCW analytical and bootstrap power/sample-size functions.
- `R/additive_stratified_analytical_app.R` and `R/additive_gam_boot_app.R`
  - additive stratified analytical functions and additive GAM/bootstrap simulation runners.
- `R/multiplicative_stratified_analytical_app.R` and `R/multiplicative_stratified_boot_app.R`
  - multiplicative stratified analytical and bootstrap method implementations.
- `R/dependent_censoring_analytical_app.R`
  - dependent-censoring analytical estimation and power/sample-size functions.
- `R/survival_diagnostics_app.R`
  - diagnostics runner used by orchestration and reporting.

### Simulation and recipe infrastructure

- `R/recipe_sim.R`
  - recipe validation, covariate generation, treatment assignment, linear predictor construction, survival-time simulation, and convenience recipe constructors.
- `R/generate_sets.R`
  - recipe grid/set expansion and metadata collection for reusable simulation suites.
- `R/load_sets.R`
  - manifest loading and manifest rebuild utilities for recipe-set assets.

### Exported API surface (high-level)

Exports (from `NAMESPACE`) are grouped as:

- Analytical/Bootstrap power and sample-size methods:
  - `linear.power.*`, `linear.ss.*`
  - `additive.power.*`, `additive.ss.*`
  - `MS.power.*`, `MS.ss.*`
  - `DC.power.*`, `DC.ss.*`
- Simulation and recipe APIs:
  - `simulate_from_recipe`, `validate_recipe`, `recipe_quick_aft`, `recipe_grid`
  - `generate_recipe_sets`, `load_recipe_sets`, `rebuild_manifest`
- Internal estimators/runners exposed for package-level composition:
  - `.estimate_*`, `.get_*_simulation_runner`, `.run_survival_diagnostics`

-----

## Data and Control Flow

1. Source selection:
- server receives either uploaded pilot data or simulated data definitions.

2. Schema and cleaning validation:
- type coercion/consistency checks are applied (time/status/arm and covariate structures).
- missingness and compatibility checks gate method execution.

3. Model selection and parameter assembly:
- server builds method-specific parameter objects from UI + data context.
- covariate definitions and allocation settings are normalized for engines.

4. Engine invocation:
- `app.R` dispatches to `R/` modules:
  - simulation path: `simulate_from_recipe(...)`
  - method paths: `*_power_*` and `*_ss_*` functions across linear/additive/multiplicative/dependent-censoring families
  - diagnostics path: `.run_survival_diagnostics(...)`

5. Result materialization:
- tabular summaries, figures, diagnostics objects, and downloadable report payloads are created.

6. Report export:
- report inputs are assembled, inline template is rendered, and HTML/PDF outputs are generated from computed state.

Recipe-set path:
- sets are generated with `generate_recipe_sets(...)` / `recipe_grid(...)`, persisted with metadata, then loaded with `load_recipe_sets(...)` for simulation execution.

-----

## Testing and Quality Map

- `tests/testthat/test-coverage-app.R`
  - app harness and broad integration coverage paths.
- `tests/testthat/test-coverage-targeted.R`
  - targeted branch and edge-condition coverage for high-risk logic.
- `tests/testthat/test-recipe-sim.R`
  - recipe simulation correctness and invariants for synthetic data generation.

Coverage policy context (`codecov.yml`):

- project and patch status thresholds are enabled (`target: auto`, `threshold: 1%`).
- precision/rounding are configured (`precision: 2`, `round: down`).
- selected paths are excluded (for example `renv/**`, `rsconnect/**`, and data-generation-specific files).

-----

## Coverage and Codecov Artifacts

Artifacts generated in `coverage/`:

- `coverage/cobertura.xml`
- `coverage/coverage.rds`
- `coverage/summary.txt`

Local artifact generation/upload helper:

```r
source("tools/run_codecov_local.R")
```

Workflow behavior in `.github/workflows/codecov_sync.yml`:

- CI checks whether `coverage/cobertura.xml` exists in the commit.
- If present, CI uploads that file to Codecov using repository secret `CODECOV_TOKEN`.
- If missing, CI skips upload and writes diagnostics into the job summary (including tracked files and directory listing).

-----

## External References

- Live application: https://arnab96.shinyapps.io/uthsc-app/
- RMSTpowerBoost package docs/source: https://uthsc-zhang.github.io/RMSTpowerBoost-Package/articles/RMSTpowerBoost-Main.html
- Project issues: https://github.com/UTHSC-Zhang/RMSTpowerBoost-Package/issues

