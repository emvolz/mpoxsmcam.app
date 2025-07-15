library( pomp )


dmeas <- pomp::Csnippet( paste( readLines(system.file('inst', 'model','dmeas5.0.c' , package='mpoxsmcam.app' ), collapse = '\n') ) 
rmeas <- pomp::Csnippet( paste( readLines(system.file('inst', 'model', 'rmeas5.0.c', package='mpoxsmcam.app' ), collapse = '\n') )
rproc <- pomp::Csnippet( paste( readLines(system.file('inst', 'model','rproc5.0.c',  package='mpoxsmcam.app' ), collapse = '\n') )
rinit <- pomp::Csnippet( paste( readLines(system.file('inst', 'model','rinit5.0.c',  package='mpoxsmcam.app' ), collapse = '\n') )

snames <- c("S"
, "E"
, "I"
, "R"
, "V"
, "Iagg" 
, "Uagg" 
, "Wagg" 
, "m1s"
, "m1e"
, "m1i"
, "m1r"
, "m1v"
, "m2s"
, "m2e"
, "m2i"
, "m2r"
, "m2v"
, "m2u" 
, "m2w" 
, "imports"
, "Phi" 
, "logrscale" 
)

pnames <- c(
"gammae" 
, "gammai" 
, "alpha" 
, "waningnat" 
, "waningvac" 
, "turnover" 
, "omega" 
, "sigma_foi" 
, "N" 
, "r" 
, "rdrift" 
, "iota0" 
, "iota1" 
, "iota2" 
, "iota3" 
, "sampf"  
, "vaccstrat"  
, "ve1" 
, "ve" 
, "initv" 
)

globs = pomp::Csnippet( glue::glue("
static double tfin = {max(d$day)};
") );

# default params 

a = 0.15 
.r = 2.0 * 1/4 / ((a+1)/a)
P0 <- c(  
	gammae = 1/8
	, gammai = 1/4 
	, alpha  = a
	, waningnat = 1/(20*365)
	, waningvac = 1/(20*365)
	, turnover  = 1/(40*365)
	, omega = 1/(2*365)
	, sigma_foi = 3.0 
	, N = 260e3
	, r = .r
	, rdrift = .05 
	, iota0 = .01 
	, iota1 = 100 
	, iota2  = 1/1 
	, iota3  = .05 
	, sampf = .33  
	, vaccstrat = 1 
	, ve1 = .36 
	, ve =  .66 
	, initv = .20 
)

M <- pomp::pomp(d
	, t0 =  -30 
	, times = 'day' 
	, globals = globs
	, rprocess = euler( rproc, delta.t = 0.1 )
	, rinit = rinit 
	, dmeasure = dmeas 
	, rmeasure = rmeas
	, covar = covariate_table( subset( vd, select=-c(X,date)) , times = 'day')
	, obsnames = c("cases", "casesbreakthrough", "casesreinf")
	, accumvars = c("Iagg", "Uagg", "Wagg" ) 
	, statenames = snames
	, paramnames = pnames
	, params = P0
)


