# ggplot2 elegan graphics for data analysis

- Grammar of graphics = an extensible, layered, formal system of independent, but
  composable static graphics components for raw data representations and statistical
  transformations
  - `data` -> `aes`thetic mapping (visualization attributes)
    - categorical variables -> color, fill, shape
    - continuous variables -> size
  - `layer` = `geom` (geometric elements) + `stat` (statistical transformations)
  - `scale` = `axes` + `legend` (data space -> aesthetic space)
  - `coord` = `axes` + `gridlines` (coordinate system)
  - `facet` split big datasets into subsets
  - `theme` = `font` + `color`
- Layer purposes
  - Display row data (gross structure, local structure, outliers, inflection points)
  - Statistical summary (model fitting and prediction) on top of raw data
  - Metadata (background context, explanative annotations, comparison references) in the
    beckground and in the foreground
