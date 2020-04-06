# IPV 0.1.0
Release

## IPV 0.1.1.
* Fixed issues due to  [changes to data.frame()](https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/index.html
).

## dev
* added option to rotate the tick label in facet charts and nested charts to avoid overlap (parameter "rotate_tick_label")
* added option to set the limit of the grid in item charts (parameter "grid_limit"). Therefore, it is now possible to generate item charts on different data with the same scaling. Note that a) the number of grid lines is determined automatically in all cases and b) values of grid_limit below the maximum center distance in the data will reduce the scope of the grid, but have no effect on the overall scaling of the chart.
