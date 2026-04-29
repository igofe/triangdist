# triangdist

This package implements the triangular distribution in R.

It includes four functions:

- `dtriang()` for the density
- `ptriang()` for the cumulative distribution
- `qtriang()` for the quantile function
- `rtriang()` for random generation

## Installation

```r
install.packages("remotes")
remotes::install_github("igofe/triangdist")
```

## Example

```r
library(triangdist)

dtriang(c(0, 0.5, 1), min = 0, max = 1, mode = 0.5)
ptriang(c(0, 0.5, 1), min = 0, max = 1, mode = 0.5)
qtriang(c(0.25, 0.5, 0.75), min = 0, max = 1, mode = 0.5)

set.seed(1)
rtriang(5, min = 0, max = 1, mode = 0.5)
```
