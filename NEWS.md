# IPV 0.1.0
Release

## IPV 0.1.1.
* Fixed issues due to  [changes to data.frame()](https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/index.html
).

## dev
* Added option to rotate the tick label in facet charts and nested charts to avoid overlap (parameter "rotate_tick_label")
* Added option to set the limit of the grid in item charts (parameter "grid_limit"). Therefore, it is now possible to generate item charts on different data with the same scaling. Note that a) the number of grid lines is determined automatically in all cases and b) values of grid_limit below the maximum center distance in the data will reduce the scope of the grid, but have no effect on the overall scaling of the chart.
* Added the option to set the order of facets for all charts types (parameter: "facet_order").
* Added the option to set the order of tests for nested charts (parameter: "test_order").
* Standardized the default order of facets across facet charts and item charts. It is now consistently the order of the correlation matrix columns in the data.
* Added a function ("rename") for convenient changing of test, facet, or item labels.
* Added the option to add an overall title for all chart types. This is only a quick and dirty shortcut. For best results (and additional options) use your typesetting software or add a title layer manually.
* Fixed a bug that caused negative correlations between facets or tests to be displayed incorrectly.
