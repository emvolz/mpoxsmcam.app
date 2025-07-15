
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


MODELCONFIGS = list(
B  = list( 
	estpars = c('alpha', 'waningnat', 'waningvac', 'r', 'sigma_foi', 'iota0')
	, P1 = c(  
			gammae = 1/8
			, gammai = 1/4 
			, alpha  = a
			, waningnat = 1/(20*365)
			, waningvac = 1/(20*365)
			, turnover  = 1/(40*365)
			, omega = 0
			, sigma_foi = 3.0 
			, N = 260e3
			, r = .r
			, rdrift = 0 
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
	, CNTRYPARMS = list( 
		NL  = list(
			PP = c(
				N = 262e3
				, vaccstrat = 1 
			)

			, eplbs  =  c(alpha=.001, waningnat=1/50/365, waningvac=1/50/365, r=.r/2, sigma_foi=3.0, iota0=.002) 
			, epubs  =  c(alpha=.025, waningnat=1/3/365,    waningvac=1/3/365   , r=.r*2, sigma_foi=5.0, iota0=0.02) 
		)
	)
	, prwsd = rw_sd(
				alpha = .02
				, waningnat = .02 
				, waningvac = .02 
				, r = .02
				, sigma_foi = .02
				, iota0 = ivp(.02)
			)
)

, B_IRD  = list( 
	estpars = c('alpha', 'waningnat', 'waningvac', 'omega', 'r', 'sigma_foi', 'iota0')
	, P1 = c(  
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
			, rdrift = 0 
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
	, CNTRYPARMS = list( 
		NL  = list(
			PP = c(
				N = 262e3
				, vaccstrat = 1 
			)
			, eplbs  =  c(alpha=.001, waningnat=1/50/365, waningvac=1/50/365, omega=1/(10*365), r=.r/2,  sigma_foi=3.0, iota0=.002) 
			, epubs  =  c(alpha=.025, waningnat=1/3/365,    waningvac=1/3/365   , omega=1/(.5*365), r=.r*2,  sigma_foi=5.0, iota0=0.02) 
		)
	)
	, prwsd = rw_sd(
				alpha = .02
				, waningnat = .02 
				, waningvac = .02 
				, omega = .02
				, r = .02
				, sigma_foi = .02
				, iota0 = ivp(.02)
			)
)

, B_PRD  = list( 
	estpars = c('alpha', 'waningnat', 'waningvac', 'r', 'rdrift', 'sigma_foi', 'iota0')
	, P1 = c(  
			gammae = 1/8
			, gammai = 1/4 
			, alpha  = a
			, waningnat = 1/(20*365)
			, waningvac = 1/(20*365)
			, turnover  = 1/(40*365)
			, omega = 0
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
	, CNTRYPARMS = list( 
		NL  = list(
			PP = c(
				N = 262e3
				, vaccstrat = 1 
			)
			, eplbs  =  c(alpha=.001, waningnat=1/50/365, waningvac=1/50/365, r=.r/2, rdrift=.001, sigma_foi=3.0, iota0=.002) 
			, epubs  =  c(alpha=.025, waningnat=1/3/365,    waningvac=1/3/365   , r=.r*2, rdrift=.15 , sigma_foi=5.0, iota0=0.02) 
		)
	)
	, prwsd = rw_sd(
				alpha = .02
				, waningnat = .02 
				, waningvac = .02 
				, r = .02
				, rdrift = .02
				, sigma_foi = .02
				, iota0 = ivp(.02)
			)
)

, B_IRD_PRD  = list( 
	estpars = c('alpha', 'waningnat', 'waningvac', "omega", 'r', 'rdrift', 'sigma_foi', 'iota0')
	, P1 = c(  
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
	, CNTRYPARMS = list( 
		NL  = list(
			PP = c(
				N = 262e3
				, vaccstrat = 1 
			)
			, eplbs  =  c(alpha=.001, waningnat=1/50/365, waningvac=1/50/365, r=.r/2, rdrift=.001, omega=1/(15*365), sigma_foi=3.0, iota0=.002) 
			, epubs  =  c(alpha=.025, waningnat=1/3/365,    waningvac=1/3/365   , r=.r*2, rdrift=.15 , omega=1/(.5*365), sigma_foi=5.0, iota0=0.02) 
		)
	)
	, prwsd = rw_sd(
				alpha = .02
				, waningnat = .02 
				, waningvac = .02 
				, r = .02
				, rdrift = .02
				, omega = .02
				, sigma_foi = .02
				, iota0 = ivp(.02)
			)
)


)
