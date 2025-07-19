library( pomp ) 
library( glue )
library( lubridate ) 
library( ggplot2 )

# - f( statelist, parmlist, cntry, nreps, horizon, ... ) 
#   simulates with modified state at april 2025 or modified parameters 
# - ribbon plots, cases 
# - histograms 
#' @export
fopl <- function(cntry, newstatelist = list(), newparmlist = list()
		 , horizon = 365*3
		 , nsim = 1e3
		 , logrlookback = 30
		 , ntrajshow = 5) 
{
	# mf, pf, d,
	mf <-  fits[[BEST]][[cntry]]  
	pf <- pfs.best[[cntry]]
	d <- ds[[cntry]]
	
	fmo <- filter_mean( pf )

	# parameters 
	P <- coef(mf) 
	# P[ 'rdrift' ] <- sd( diff( tail(fmo['logrscale',],logrlookback))  ) 
	for ( pn in names( newparmlist ) ){
		P[ pn ] <- newparmlist[[ pn ]]
	}
	## fix r  
	P['rdrift'] = 0
	# get last state
	x1 <- tail(t(fmo),1) |> as.vector() |> setNames(rownames(fmo))
	# new pomp
	.rinit <- function(parameters, ... ){
	    	x2 <- x1 
	    	x2[ 'logrscale'] <- (fmo['logrscale', ] |> tail(logrlookback) |> median() )
	    	for ( xn in names(newstatelist)){
	    		x2[ xn ] <- newstatelist[[ xn ]]
	    	}
	    	x2 
	}
	t10 <- max(d$day) 
	d1 <- data.frame( day = seq( t10, t10 + horizon, by = 7 )
		, cases = NA 
	)
	# redefine for partrans 
	globs = Csnippet( glue::glue("
	static double tfin = {max(d$day)};
	") );
	M1 <- pomp::pomp(d1
		, t0 =  t10 
		, times = 'day' 
		, globals = globs
		, rprocess = euler( rproc, delta.t = 0.1 )
		, rinit = .rinit 
		, dmeasure = dmeas 
		, rmeasure = rmeas
		, covar = covariate_table( subset( vd, select=-c(X,date)) , times = 'day')
		, obsnames = c("cases", "casesbreakthrough", "casesreinf")
		, accumvars = c("Iagg", "Uagg", "Wagg" ) 
		, statenames = snames
		, paramnames = pnames
		, params = P
	)
	# simulate new taxis 
	s = simulate( M1 , nsim = nsim)
	sdf <- as.data.frame( s ) 
	# sdf$cases |> print() 
	d$date <- as.Date( d$date )
	fodaxis = max(d$date) + d1$day - t10
	fotaxis = decimal_date( fodaxis )
	simout = list( s = s, sdf = sdf, fodaxis = fodaxis, fotaxis = fotaxis, d =d )
	focases = with( simout, do.call( cbind, lapply( split( sdf, sdf$.L1 ) , function(dd) dd$cases )) )
	focases 
	fobnds <- apply( focases, MAR = 1, function(x)  quantile(x, c( .5, .025, .975)))
	fomaxes <- apply( focases, MAR = 2, max )
	fosums <- colSums( focases )
	
	fmo <- filter_mean( pf ) 
	mcases <- fmo[ 'Iagg', ]
	pldf <- data.frame( 
		central=c( qbinom( .5, mcases, prob = P['sampf']) ,  fobnds[1,] )
		, lb = c(qbinom( .025, mcases, prob = P['sampf']), fobnds[2,] ) 
		, ub = c(qbinom( .975, mcases, prob = P['sampf']), fobnds[3,] ) 
		, cases = c( d$cases, d1$cases  )
		, day = c( d$day, d1$day  )
		, date = c( d$date, fodaxis )
		, fo = c( rep(FALSE, nrow(d)), rep(TRUE, nrow(d1)))
	)

	pl = ggplot(pldf, aes(x = date, y = central, ymin=lb, ymax=ub) ) + 
		geom_path() + 
		geom_ribbon(alpha=.2) + 
		geom_point(aes(x=date,y=cases)) + 
		theme_classic() + xlab('') + ylab('') 
	if ( ntrajshow > 0 )
	{
		fos <- focases[, sample.int(ncol(focases),replace=FALSE,size = round(ntrajshow)) ]
		fosdf <- do.call( rbind, lapply( 1:ncol(fos), function(i){
			data.frame( replicate = as.character(i) , central = fos[,i] , date = fodaxis, lb = fos[,i], ub = fos[,i], cases  = NA, day = NA, fo = TRUE ) 
		}))
		pl <- pl + geom_path( data = fosdf, aes(x = date, y = central, colour = replicate) , linewidth=.5, alpha = 1, lty = 1)
	}

	plmax <- ggplot( data.frame(maxcases = fomaxes) , aes(x=maxcases)) + 
		geom_histogram(aes(y=after_stat(density)) 
			,  fill = 'blue1', colour = 'white', alpha  = .5) +
		geom_density( colour = 'darkblue', linewidth=2) + 
		labs( x= glue::glue('Forecast max cases ({horizon} days)'), y = 'Density' ) + 
		theme_classic()

	plsum <- ggplot( data.frame(cumulativecases = fosums) , aes(x=cumulativecases)) + 
		geom_histogram(aes(y=after_stat(density))
			, fill = 'blue1', colour = 'white', alpha  = .5) +
		geom_density( colour = 'darkblue', linewidth=2) + 
		labs( x= glue::glue('Forecast cumulative cases ({horizon} days)'), y = 'Density' ) + 
		theme_classic()


	list( simout = simout , pldf = pldf 
		, plot = pl
		, plotmax = plmax 
		, plotsum = plsum 
	)
}

if (FALSE)
{

	library( pomp ) 
	library( glue )
	library( lubridate ) 
	library( ggplot2 )
	library( mpoxsmcam.app )

	o = fopl('ES', newstatelist = list(), newparmlist = list(),  horizon = 365*3, nsim = 1e2) 
	o = fopl('NL', newstatelist = list(m1s=1), newparmlist = list(),  horizon = 365*3, nsim = 1e2) 
	o$plot
	o$plotmax
	o$plotsum

	o = fopl('NL', newstatelist = list(m1s=1.0), newparmlist = list(),  horizon = 365*3, nsim = 1e2) 
	o = fopl('NL', newstatelist = list(m1s=.1), newparmlist = list(),  horizon = 365*3, nsim = 1e2) 
	o = fopl('NL', newstatelist = list(m1s=.5), newparmlist = list(gammai=1/20),  horizon = 365*3, nsim = 1e2) 
	o$plot
}
