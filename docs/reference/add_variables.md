# Check a data frame for the existence of columns. If not present, create variables with a default value.

Check a data frame for the existence of columns. If not present, create
variables with a default value.

## Usage

``` r
add_variables(df, varnames, default = NA)
```

## Arguments

- df:

  Data frame that is checked (and optionally expanded with new
  variables)

- varnames:

  The variable names that are searched in the df

- default:

  The default value for newly created columns
