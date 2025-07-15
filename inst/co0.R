library( mpoxsmcam.app )

cargs <- commandArgs(trailingOnly = TRUE )
dxs <- as.numeric( cargs[1] )

cat( '======================\n') 
print( dxs )

o = .compute_elasticities(  dxs = dxs, Np = 5e4, nrep = 7, ncpu = 8)

write.csv( o , file = glue::glue('elasticity0-{cargs[1]}.csv' ))
