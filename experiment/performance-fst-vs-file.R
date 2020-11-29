# remotes::install_github('dipterix/lazyarray@filearray')

require(lazyarray)

path1 <- tempfile()
path2 <- tempfile()

dim <- c(100, 100, 100, 100)
data <- rnorm(prod(dim))
pryr::object_size(data)

fstarray <- fstarray(path1, dim)
filearray <- lazyarray(path2, dim)

fstarray[] <- data
filearray[] <- data

system.time({fstarray[,,,]})

#  user  system elapsed 
# 2.380   1.052   1.296

system.time({filearray[,,,]})

#  user  system elapsed 
# 0.377   0.322   0.281


# range(fstarray[,,,] - filearray[,,,])
lazyarray:::setLazyBlockSize(100)
