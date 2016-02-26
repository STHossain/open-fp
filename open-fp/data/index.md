---
layout: page
title: Data
permalink: /data/
---

On this page you can download all submitted projections. Currently, all forecasts are included in one data set. You can download it in various formats (selective download is on our road-map). The script for gathering the forecasts from external sources can be found [here](https://github.com/onnokleen/open-fp).

<!--IDEA: Insert Shiny app for download, so that everyone gets the newest version for his  (maybe not such a good idea - links are not accessible via https).-->

* One to serve them all: ([forecast.panel.csv](https://open-fp.org/data/forecast.panel.csv))
* R ([forecast.panel.rds](https://open-fp.org/data/forecast.panel.rds))
* Stata ([forecast.panel.dta](https://open-fp.org/data/forecast.panel.dta))

If you use our data-file, please cite as ...

The data set contains one matrix, which columns contain the following information:

1. Forecaster related information
* Forecaster ID (forecaster.id)
* Name of the forecaster if provided (forecaster.name)
* Affiliation of the forecaster if provided (forecaster.affiliation)
* Panel (panel), for example Open-FP, SPF-ECB
* Panel ID (panel.id)
2. Forecast meta information
* Macroeconomic variable (variable)
* Country or region (region)
* Date of submission (submission.date)
* Frequency of forecasts, i.e. monthly, quarterly... (forecast.frequency)
* Issued period, for example 2016Q1 (issued.period)
* The date when the forecast was generated (date.forecast.generated)
* Target period, for example 2017Q1 (target.period)
* Steps ahead (steps.ahead)
* Forecating method (forecasting.method)
* Binary variable for panel data whether we have a fixed horizon or fixed event forecast (fixed.event.or.horion).
3. Point forecasts (automatically generated if a distribution forecast has been submitted)
* Point forecast (point.forecast)
* Forecast uncertainty given by the variance (variance.point.forecast)

4. Distribution forecast
* Distribution (forecast.distribution): custom, normal, beta, t, two-piece normal
* Distribution parameters: normal.mean, normal.variance, t.degrees.of.freedom, ...
* Custom distribution (automatilly generated if you submit a non-custom distribution and its parameters)
5. Evaluation
* Loss function for point forecasts (loss.function.point.forecast, default: [MSE](#MSE))
* Binary variable whether the loss function has been specified by the forecaster (binary.loss.function.point.forecast)
* Loss function for distribution forecasts (loss.function.distribution.forecast, default: [CRPS](#CRPS))
* Binary variable whether the loss function has been specified by the forecaster (binary.loss.function.distribution.forecast)

## Loss functions

The following loss functions can be selected during the submitting process.

### Point forecasts

#### Mean squared error loss (MSE)
<a name="MSE"></a>

The mean squared error loss for the predictions
$$
f_{i,t}
$$
of forecaster
$$
i
$$ is defined as 

$$
\frac{1}{T} \sum_{t = 0}^T (f_{i, t}-x_{t})^2.
$$

### Distribution forecasts

#### Continuous ranked probability score (CRPS)
<a name="CRPS"></a>

The mean continuous ranked probability score is defined as

$$
\frac{1}{T} \sum_{t = 0}^T \int (F(x_t )-1 (x > x_t)) \ \text{d}x
$$

