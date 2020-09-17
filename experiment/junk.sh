devtools::load_all(); 
dim = c(2,3,4,5)
x = array(1:(prod(dim)), dim)# + 1i * array(rnorm(prod(dim)), dim)
a = as.lazyarray(x, path = tempfile())
idx <- c(1:120, 1:120)#sample(length(x), size = 200, replace = TRUE)
# idx[sample(200, 20)] = NA
re <- a[idx]
cp <- x[idx]
expect_equivalent(re, cp)

tik()
gctorture2(1)
a[1:2] -> re

# a[] -> re
gctorture(FALSE)
as.data.frame(tok("", stop=TRUE))
