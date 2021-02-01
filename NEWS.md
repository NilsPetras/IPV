# IPV 0.2.0

## Added Features
* Package now works based on raw data with automated model estimation using lavaan (function `ipv_est`). This is now the recommended workflow!
* New raw example data (`HEXACO`) from the open psychometrics project (\url{https://openpsychometrics.org/}) (see `?HEXACO`).
* Create an overview over all (squared) factor loadings in all three models on which a nested chart is based (function `item_overview`).
* Function for convenient changing of test, facet, or item labels (function `rename`).
* Rotate the tick label in facet charts and nested charts to avoid overlap (parameter `rotate_tick_label`).
* Set the limit of the grid in item charts (parameter `grid_limit`).
* Set the order of facets for all charts types (parameter: `facet_order`).
* Set the order of tests for nested charts (parameter: `test_order`).
* Add an overall title for all chart types (parameter `title`).
* Draw an xarrow in a nested chart that end on a test circle (instead of a facet circle) if `facet1`or `facet2` is specified as `NA` for that arrow.
* Overhaul of the documentation in the Vignette.

## Bugfixes
* Standardized the default order of facets across facet charts and item charts. It is now consistently the order of the correlation matrix columns in `data = `.
* Negative correlations between facets or tests should now be displayed correctly.
* Removed an erroneous message claiming that the axis tick was set automatically when it was set manually.
* The axis tick mark (gray dotted circle) now disappears "behind" circles of facet charts and nested charts, instead of cutting through them.
* the axis tick label (number) now more reliably appears close to the axis tick mark in facet charts and nested charts.

# IPV 0.1.0
Release

## IPV 0.1.1.
* Fixed issues due to  [changes to data.frame()](https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/index.html
).

