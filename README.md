# xmod: Collection of models for expected/excess death calculation

![github tag](https://img.shields.io/github/v/tag/jschoeley/xmod?style=flat-square)

![](ass/cover.png)

## Introduction

R-Implementations of a range of expected/excess death models. Forecast time series of monthly/weekly death counts taking into account seasonality, temperature, public holidays, population structure and long-term mortality trends. Example data from [MOCY](https://github.com/jschoeley/mocy).

## Prerequisites

As a pre-requisite to running this locally, you will need a working installation of [**R**](https://www.r-project.org/) with all of the necessary dependencies (see `./src/install_dependencies.R`).

## Structure

- `./dat` input source data
  - `weekly_mortality_cv.rds`
- `./src` code to replicate the data cleaning, analysis and visualization
- `./ass` repository assets
