# safir2

<!-- badges: start -->
[![R-CMD-check](https://github.com/mrc-ide/safir2/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/safir2/actions)
[![Codecov test coverage](https://codecov.io/gh/mrc-ide/safir2/branch/main/graph/badge.svg)](https://codecov.io/gh/mrc-ide/safir2?branch=main)
<!-- badges: end -->

__safir2__ is an upgraded and refactored version of [__safir__](https://github.com/mrc-ide/safir), which implements a stochastic individual
based model of SARS-CoV-2 transmission. The model takes into account individual level
immunity and neutralizing antibody titre, as well as allowing for complex vaccine
allocation schemes. It uses [__individual__](https://github.com/mrc-ide/individual)
to specify and run the simulation model.

## Installation

```r
remotes::install_github('mrc-ide/safir2')
library(safir2)
```

## License

MIT © Imperial College of Science, Technology and Medicine
