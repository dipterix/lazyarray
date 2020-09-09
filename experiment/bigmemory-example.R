path <- '~/Desktop/junk/'
f <- 'bigmemory-ieeg.testfile'
# unlink(file.path(path, f))
dir.create(path)
ncol = 84
x <- bigmemory::attach.resource(file.path(path, 'bigmemory-ieeg.testfile.desc'))
# x <- big.matrix(34497400, ncol = ncol, type = 'double', backingfile = f, backingpath = path)
# for(ii in 1:ncol){
#   x[,ii] <- y[,ii]
# }
system.time({x[]})
