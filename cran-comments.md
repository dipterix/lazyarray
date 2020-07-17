Current submission 1.1.0:

Self check, 0 errors, 0 warnings, 0 notes

Changes:
* Explicitly import `R6Class` to clear the note on [CRAN check website](https://cran.r-project.org/web/checks/check_results_lazyarray.html)
* Added `little-endian platform` to `SystemRequirements`
* Added the compression method and backend into `DESCRIPTION`: 
> The internal storage format is provided by 'fstcore' package geared by 'LZ4' and 'ZSTD' compressors.


Last submission 1.0.0: passed

Comments
```
For your next update:
If there are references describing the (theoretical background of)
methods or algorithms in your package, please add these in the
Description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.
```

