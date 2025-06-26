# ggplotUtils

Enhanced utilities for creating publication-ready plots with ggplot2.

## Installation

You can install the development version of ggplotUtils from GitHub:

```{r, eval = FALSE}
# install.packages("devtools")
devtools::install_github("yourusername/ggplotUtils")
```
## Example

```{r, eval = FALSE}
library(ggplotUtils)
library(ggplot2)

# Create sample data
df <- data.frame(
  x = rep(c("A", "B", "C"), each = 10),
  y = c(rnorm(10, 5), rnorm(10, 7), rnorm(10, 6))
)

# Create modular bar plot
p <- create_barplot(df)
p <- add_errorbars(p)
p <- apply_minimal_theme(p, title = "My Plot")
print(p)
```

##  Build and Check Package

```r
# Check package for errors
devtools::check()

# Build package documentation
devtools::document()

# Install package locally
devtools::install()
```