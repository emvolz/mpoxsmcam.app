cntrys1 <- c( 'NL',  'IE', 'ES'  )


a = 8.399733e-01 
P0 <- c(  
	gammae = 1/8
	, gammai = 1/4 
	, alpha  = a
	, waningnat =1.055033e-04 
	, waningvac =2.483118e-05
	, turnover  = 1/(40*365)
	, omega =2.162888e-03 
	, sigma_foi =1.180671e+00 
	, N = 260e3
	, r =3.216017e-01 
	, rdrift =7.959029e-02  
	, iota0 =1.296769e-02  
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
B_IRD_PRD2  = list( 
	estpars = c('alpha', 'omega', 'r', 'rdrift', 'sigma_foi', 'iota0')
	, P1 = c(  
		gammae = 1/8
		, gammai = 1/4 
		, alpha  = a
		, waningnat =1.055033e-04 
		, waningvac =2.483118e-05 
		, turnover  = 1/(40*365)
		, omega =2.162888e-03 
		, sigma_foi =1.180671e+00 
		, N = 260e3
		, r =3.216017e-01 
		, rdrift = 7.959029e-02 
		, iota0 = 1.296769e-02 
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
			, eplbs  =  c(alpha=.001, r=.r/2, rdrift=.001, omega=1/(15*365), sigma_foi=3.0, iota0=.002) 
			, epubs  =  c(alpha=.025, r=.r*2, rdrift=.15 , omega=1/(.5*365), sigma_foi=5.0, iota0=0.02) 
		) 
		, ES = list(
			PP = c(
				N = 723e3
				, vaccstrat = 1 
			)
			, eplbs  =  c(alpha=.001, r=.r/2, rdrift=.001, omega=1/(15*365), sigma_foi=3.0, iota0=.002) 
			, epubs  =  c(alpha=.025, r=.r*2, rdrift=.15 , omega=1/(.5*365), sigma_foi=5.0, iota0=0.02) 
		)
		, IE = list(
			PP = c(
				N = 43e3
				, vaccstrat = 1 
			)
			, eplbs  =  c(alpha=.001, r=.r/2, rdrift=.001, omega=1/(15*365), sigma_foi=3.0, iota0=.002) 
			, epubs  =  c(alpha=.025, r=.r*2, rdrift=.15 , omega=1/(.5*365), sigma_foi=5.0, iota0=0.02) 
		)	)
	, prwsd = rw_sd(
				alpha = .02
				, r = .02
				, rdrift = .02
				, omega = .02
				, sigma_foi = .02
				, iota0 = ivp(.02)
			)
)


)

