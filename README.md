# Compare the order of lists
An R package to compare ranked lists of stuff.

## Install
From within R, use:
```R
# install.packages("devtools") # If not installed yet
devtools::install_github("TCP-Lab/compare_ranks")
```
## Usage
This package does not have many functions, however, they are all documented and include usage examples.
Check `??<function>` to learn more.
In short:
- `window_compare_rank`: Compare the order of the items in two vectors using a rolling window;
- `continuous_congruency`: Compute the contiguous congruency metric of two vectors.
- `plot_continuous_congruency`: Create a `ggplot2` of the results of one or more continuous congruency comparisons
- `compare_two_way_ranks`: Compare all ranks in a list taken two at a time
- `score_comparisons`: Score all two-way comparisons (e.g. created by `compare_two_way_ranks`)
- `test_comparisons`: Use a non-parametric, permutation-based approach to compute a p-value of the difference in ordering between two vectors a and b, with null hypothesis that the vectors are of different order.
