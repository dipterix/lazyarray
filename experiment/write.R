f = tempfile()
data = list(
  a = 1:10,
  b = 1:2
)


.Call(fstcore:::`_fstcore_fststore`, normalizePath(f, mustWork = FALSE), data, 20L, TRUE)
tbl = fst::fst(f)
tbl





