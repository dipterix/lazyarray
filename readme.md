# Lazyarray - R package to store/read large array on hard drive

Zhengjia Wang

## Overview

The `lazyarray` package for R provides a fast way to serialize arrays. With reading speeds of Giga-byte levels, `lazyarray` unlocks the potential of high speed solid state drive.

The figure below compares the read performance of the `lazyarray` package to alternatives. Original array size is 1 GB. 

|                     |  lazyarray (4 CPU)|  lazyarray (1 CPU) | readRDS|
|:--------------------|----------:|--------:|--------:|
|Read the entire array|      1.2 s|    1.6s |    6.1 s|
|Read 300 MB slice    |      0.5 s|    0.8s |    6.2 s|

With multi-thread (OpenMP) support, the reading speed can be even faster. Since all subset process are handled within C++ code, when slicing partial data, `lazyarray` calculates indices first, and only read in the slice itself while `readRDS` needs to load all data first and slice the data. This results in a more memory-efficient approach when only partial data are needed.

## Installation

```r
install.packages("lazyarray")
```

## Basic usage

```r
library(lazyarray)

# Sample data (~240 MB)
x <- rnorm(3e7); dim(x) <- c(100, 100, 100, 30)

# Save array to a path
path <- tempfile()
arr <- create_lazyarray(path, 'double', dim(x), multipart = TRUE)
arr[] <- x

# Subset/read array
y1 <- arr[]              # 400 ms
y2 <- arr[,,,3]          # 45 ms

# Load existing array
arr <- load_lazyarray(path)

# Make loaded array writable
arr$make_writable()
arr$can_write

# Write to slice of data, writing to slices along the 
# last dimension is optimized
arr[,,,1] <- seq_len(1e6)
```

## Multipart array and its shared property

Lazy arrays can be shared across multiple R sessions and is compatible with `future` package. For example, we want to perform some complicated tasks along the last dimension. Normally, we can use `sapply` to interate through each slices as:

```r
t(sapply(1:10, function(i){
  slice <- arr[,,,i]
  Sys.sleep(1)  # simulate "complicated" tasks
  c(mean(slice), median(slice))
}))
```

This process needs around *10.8 seconds* to finish. With multi-core support, the task can be speed up:

```r
# install.packages('dipsaus')
library(dipsaus)

mean_median <- lapply_async2(1:10, function(i){
  slice <- arr[,,,i]
  Sys.sleep(1)  # simulate "complicated" tasks
  c(mean(slice), median(slice))
}, workers = 4L)
do.call('rbind', mean_median)
```

It only takes about *3.4 seconds* with 4 cores.

## Remove Arrays

Data created via `lazyarray` package does not unlink automatically. You need to finalize array by yourself. This is because multiple lazy array instances might point to a same dataset. If one of the object is garbage collected, you might not want to remove the data on hard drive as this will invalidate the other instances. 


To manually remove data, use

```r
arr$remove_data()
```

Automatically removing data when array is not used could be tricky. However, package `dipsaus` provides a a worry-free solution.

```r
# install.packages('dipsaus')
library(dipsaus)

# key must be the same for the array
shared_finalizer(arr, key = arr$storage_path, function(e){
  e$remove_data(force = TRUE)
})
```

Here's a demonstration on how to register multiple arrays

```r
# install.packages('dipsaus')
library(dipsaus)

# Register shared finalizer
fin <- function(e){
  e$remove_data(force = TRUE)
}
arr <- load_lazyarray(path)
shared_finalizer(arr, key = arr$storage_path, fin)


arr_another <- load_lazyarray(path)
shared_finalizer(arr_another, key = arr$storage_path, fin)

# now removing arr_another still keeps data
rm(arr_another); gc()

# To test if array is still valid
arr[1:10,1,1,1]

# removing arr will trigger 
rm(arr); gc()

# path is removed
dir.exists(path)   # FALSE

```
