#

#' @export 
.compute_elasticities <- function( ps = NULL, dxs = c( .01,.02, .04, .08, .16 ),  Np = 1e4, nrep = 5, ncpu = 8, cntrys = c('NL') )
{
	if ( is.null( ps ))
		ps <- mpoxsmcam.app::psumdf$parameter[ mpoxsmcam.app::psumdf$Elasticity ] 
	for ( cntry in cntrys )
	{
		mf <-  mpoxsmcam.app::fits[[BEST]][[cntry]]  
		pf <- mpoxsmcam.app::pfs.best[[cntry]]
		PP <- coef( mf )
		L <- logLik( pf )

		countries <- c()
		parameter <- c()
		dX <- c()
		dloglikelihood0 <- c()
		dloglikelihood1 <- c()

		for ( xn in ps )
		{
			for ( dx in dxs )
			{
				PPd <- PP 
				PPd[xn] <- coef(mf)[xn]*(1-dx)
				PPu <- PP 
				PPu[xn] <- coef(mf)[xn]*(1+dx)
				lld <- .fmll( mf, P2 = PPd , Np = Np, nrep = nrep, ncpu = ncpu)
				llu <- .fmll( mf, P2 = PPu , Np = Np, nrep = nrep, ncpu = ncpu)
				
				countries <- c( countries , cntry )
				parameter <- c( parameter, xn )
				dX <- c( dX, dx )
				dloglikelihood0 <- c( dloglikelihood0, L - lld[1] )
				dloglikelihood1 <- c( dloglikelihood1, L - llu[1] )

			}

		}

	}
	data.frame( 
		countries  = countries   
		, parameter  = parameter   
		, dX  =        dX  
		, dloglikelihoodd0  = dloglikelihood0
		, dloglikelihoodd1  = dloglikelihood1
	)
}

if (FALSE)
{

library( mpoxsmcam.app )
library( pomp ) 
library(foreach)
library(doParallel)
library( iterators )

o = .compute_elasticities( ps = 'alpha', dxs = .5, Np = 2e3, nrep = 5, ncpu = 8)

}
