
int ia = nearbyint( Iagg ); 
// int iu = nearbyint( Uagg ); 
// int iw = nearbyint( Wagg ); 
double penltycoef = 10.0; 
double xc = cases - Iagg ; 
xc = (xc > 0.0) ? xc : 0.0; 
ia = (ia>cases) ? ia : cases; 

double omegadiff = (log(omega)-log(2.0/365.0));
double omegalogit = exp(50*omegadiff)/(1+exp(50*omegadiff));  
double omegapnlty = 100.0 * omegalogit; 

double waningnatdiff = (log(waningnat)-log(1/(10*365.0)));
double waningnatlogit = exp(50*waningnatdiff)/(1+exp(50*waningnatdiff));  
double waningnatpnlty = 100.0 * waningnatlogit; 

double waningvacdiff = (log(waningvac)-log(1/(2.0*365.0)));
double waningvaclogit = exp(50*waningvacdiff)/(1+exp(50*waningvacdiff));  
double waningvacpnlty = 100.0 * waningvaclogit; 

double alphadiff = (log(alpha)-log(1.0));
double alphalogit = exp(50*alphadiff)/(1+exp(50*alphadiff));
double alphapnlty = 100.0 * alphalogit; 

double p[] = { (Iagg-Uagg-Wagg)/(Iagg+1e-6), Uagg/(Iagg+1e-6), Wagg/(Iagg+1e-6)}; 
double x_values[] = {(double)cases, (double)casesbreakthrough, (double)casesreinf};

lik = dbinom( cases, nearbyint(ia), sampf, give_log )
	+ dmultinom(3, p, x_values, give_log)
	- penltycoef * xc 
	- waningnatpnlty
	- waningvacpnlty
	- alphapnlty
	- omegapnlty; 
if (isnan( lik )) lik = R_NegInf; 
if (!give_log) lik = 0.0; // not impl 
// if (give_log) lik = log(lik) ; 
