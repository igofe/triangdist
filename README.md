# triangdist

`triangdist` implements the triangular distribution in R. It provides density,
distribution, quantile, and random-generation functions with vectorized
parameters.

## Installation

Install the development version from GitHub with:

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
