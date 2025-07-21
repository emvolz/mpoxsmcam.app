#

#' @export 
.compute_elasticities <- function( ps = NULL, dxs = c( .01,.02, .04, .08, .16 ),  Np = 1e4, nrep = 5, ncpu = 8, cntrys = c('NL') )
{
	if ( is.null( ps ))
		ps <- mpoxsmcam.app::psumdf$parameter[ mpoxsmcam.app::psumdf$Elasticity ] 
	for ( cntry in cntrys )
	{
		mf <- mpoxsmcam.app::fits[[BEST]][[cntry]]  
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

#' @export 
edf <- read.csv( system.file('elasticity0.csv', package = 'mpoxsmcam.app'  ) ) 
edf$parameter <- as.factor( edf$parameter )

#' @export 
# medf0 <- lm( abs(dloglikelihoodd0) ~ parameter + parameter:dX, data = edf )
medf0 <- mgcv::gam( abs(dloglikelihoodd0) ~ parameter + s(dX,by=parameter,k=3), data = edf, family = Gamma )

#' @export 
# medf1 <- lm( abs(dloglikelihoodd1) ~ parameter + parameter:dX, data = edf )
medf1 <- mgcv::gam( abs(dloglikelihoodd1) ~ parameter + s(dX,by=parameter,k=3),  data = edf, family = Gamma )
 


#' @export 
dlikelihoodtable <- function(dx)
{
	# dx = .05
	epars <- psumdf$parameter[ psumdf$Elasticity]

	pdx <- predict( medf0, newdata = data.frame( dX = dx, parameter = epars ) )
	pd0 <- predict( medf0, newdata = data.frame( dX = 0, parameter = epars ) )
	dy = abs( pdx - pd0 ) |> setNames( epars )
	odf0 <- data.frame( perturbation = dx , parameter = epars, dloglikelihood = dy , direction ='downwards' )

	pdx <- predict( medf1, newdata = data.frame( dX = dx, parameter = epars ) )
	pd0 <- predict( medf1, newdata = data.frame( dX = 0, parameter = epars ) )
	dy = abs( pdx - pd0 ) |> setNames( epars )
	odf1 <- data.frame( perturbation = dx , parameter = epars, dloglikelihood = dy , direction ='upwards' )

	odf <- rbind( odf0, odf1  )
	odf$alias <- rep(psumdf$alias[ psumdf$Elasticity ], 2)
	odf$Estimated <- rep(psumdf$Estimated[ psumdf$Elasticity ], 2)
	odf
}

if (FALSE)
{

library( mpoxsmcam.app )
library( pomp ) 
# Optional parallel processing libraries loaded conditionally in .fmll function

o = .compute_elasticities( ps = 'alpha', dxs = .5, Np = 2e3, nrep = 5, ncpu = 8)

}

if (FALSE)
{
	library( ggplot2 )
	ggplot( edf, aes(dX, abs(dloglikelihoodd0)) ) + geom_point() + facet_wrap(~parameter,scales='free_y') 
	ggplot( edf, aes(dX, abs(dloglikelihoodd1)) ) + geom_point() + facet_wrap(~parameter,scales='free_y') 

}
