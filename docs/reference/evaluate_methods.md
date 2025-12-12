# Evaluate predicted anomaly segments

Computes precision, recall, F1, and intersection-over-union (IoU) for
predicted vs. reference segments given a time tolerance.

## Usage

``` r
evaluate_methods(
  pred,
  truth,
  tolerance = "10 min",
  method = NULL,
  runtime_sec = NULL
)
```

## Arguments

- pred, truth:

  Data frames/tibbles with columns `id`, `start`, and `end`. `pred` can
  also be a named list of prediction objects (see Details).

- tolerance:

  Allowed temporal slack interpreted by
  [`base::as.difftime()`](https://rdrr.io/r/base/difftime.html).

- method:

  Optional label applied when `pred` is a single data frame. When `pred`
  is a list, method labels default to list names or entry metadata.

- runtime_sec:

  Optional runtime metadata (in seconds) for the supplied predictions.
  When `pred` is a list, runtime metadata can also be provided per entry
  (defaults to `NA`).

## Value

A tibble with columns `method`, `runtime_sec`, `mean_n_cps`,
`precision`, `recall`, `f1`, `mae_cp`, and `iou`.
