# Wilcoxon test operator

##### Description

The `wilcoxtest` operator performs a Wilcoxon test on the data.

##### Usage

Input projection|.
---|---
`y-axis`| measurement value
`color`   | represents the groups to compare
`labels`   | optional, represents the pairing for paired tests
`row`   | optional, row stratification factor
`column`   | optional, column stratification factor

Input parameters|.
---|---
`alternative`   | A character string specifying the alternative hypothesis, default is "two.sided"
`paired`   | logical, indicating whether to perform pairing, default FALSE
`mu`  | A number indicating the true value of the mean (or difference in means if you are performing a two sample test), default 0.0
`conf.level`  |numeric, confidence level of the interval, default 0.95

Output relations|.
---|---
`pv`| numeric, p-value calculated per cell

##### Details

The operator is the `wilcox.test` function in base R.

##### References

see the `base::wilcox.test` function of the R package for the documentation. 

##### See Also

[kruskaltest](https://github.com/tercen/kruskaltest_operator), [ttest](https://github.com/tercen/ttest_operator)
