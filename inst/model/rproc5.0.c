
const double M = 1.0 ; 
double popM2 = pow(1/alpha,2) * alpha * (alpha + 1 ); 



if (((int)t) % 30 == 0) {
  logrscale += rnorm(0.0, rdrift); 
}
double rscale = exp( logrscale ); 
double rt = r * rscale ; 

double di = dt * rt * m1s * m1i / M ;
di  = (di > 0) ? di : 0.0; 


double var_foi = pow( sigma_foi,2)/(di*N) ; 
di *= (var_foi < 1.0e-6) ? 1.0 : rgamma( 1/var_foi, var_foi );



double newimports = (dt*m1s/(N*M))*(iota0 + iota1*Phi) + iota3*di ; 

double var_foi_imports = pow( sigma_foi,2)/(newimports*N) ;
newimports *= (var_foi < 1.0e-6) ? 1.0 : rgamma( 1/var_foi_imports, var_foi_imports ); 
di += newimports ; 




double mnewinf = m2s; 
double m3s = m2s*m1s+2*alpha*m2s; 


double m1s_m1e = di * mnewinf; 
double m2s_m2e = di * m3s;  
double s_e = di ; 


double vr = 0.0; 
double m1s_m1v = 0.0; 
double m2s_m2v = 0.0; 
double s_v = 0.0; 
vr = (FirstDoses/N)*ve1;
vr += (SecondDoses/N)*(1-ve1)*ve; 
s_v = dt * vr * S; 


double vru = ( (FirstDoses/N)*(1.0-ve1) ); 

double m2s_m2u_nme = 0.0; 
if (vaccstrat == 0 ){
	m1s_m1v = dt * vr * m1s; 
	m2s_m2v = dt * vr * m2s;
	m2s_m2u_nme = dt*vru*m2s ; 
}  else if ( vaccstrat == 1 ){
	m1s_m1v = dt * vr * mnewinf; 
	m2s_m2v = dt * vr * m3s; 
	m2s_m2u_nme = dt*vru*m3s ; 
} else{
	Rprintf( "Invalid vaccstrat" ); 
}




double m1e_m1i = dt * gammae * m1e; 
double m1i_m1r = dt * gammai * m1i; 
double m2e_m2i = dt * gammae * m2e; 
double m2i_m2r = dt * gammai * m2i; 
double e_i = dt * gammae * E; 
double i_r = dt * gammai * I; 


double m1r_m1s = dt * waningnat * m1r; 
double m2r_m2s = dt * waningnat * m2r; 
double m1v_m1s = dt * waningvac * m1v; 
double m2v_m2s = dt * waningvac * m2v; 
double r_s = dt * waningnat * R; 
double v_s = dt * waningvac * V; 




double irdm1s = dt * omega*(S*M-m1s); 
double irdm2s = dt * omega*(S*popM2-m2s); 

double irdm1e = dt * omega*(E*M-m1e); 
double irdm2e = dt * omega*(E*popM2-m2e); 

double irdm1i = dt * omega*(I*M-m1i); 
double irdm2i = dt * omega*(I*popM2-m2i); 

double irdm1r = dt * omega*(R*M-m1r); 
double irdm2r = dt * omega*(R*popM2-m2r); 

double irdm1v = dt * omega*(V*M-m1v); 
double irdm2v = dt * omega*(V*popM2-m2v); 




double m1s_0 = dt * turnover*m1s; 
double m1e_0 = dt * turnover*m1e; 
double m1i_0 = dt * turnover*m1i; 
double m1r_0 = dt * turnover*m1r; 
double m1v_0 = dt * turnover*m1v; 

double m2s_0 = dt * turnover*m2s; 
double m2e_0 = dt * turnover*m2e; 
double m2i_0 = dt * turnover*m2i; 
double m2r_0 = dt * turnover*m2r; 
double m2v_0 = dt * turnover*m2v; 
double m2u_0 = dt * turnover*m2u;
double m2w_0 = dt * turnover*m2w;

double null_m1s = dt * turnover*M; 
double null_m2s = dt * turnover*popM2; 

double S_null = dt * turnover*S; 
double E_null = dt * turnover*E; 
double I_null = dt * turnover*I; 
double R_null = dt * turnover*R; 
double V_null = dt * turnover*V; 
double null_S = dt * turnover; 



double pu  = m2u / m2s ; 
pu = (pu < 0) ? 0.0 : pu; 
pu = (pu > 1) ? 1.0 : pu; 
double m2u_m2e = pu * m2s_m2e ; 
double pw = m2w / m2s ; 
pw = (pw < 0) ? 0.0 : pw; 
pw = (pw > 1) ? 1.0 : pw; 
double m2w_m2e = pw * m2s_m2e ; 
double m2v_m2u = waningvac * m2v; 
double m2r_m2w = waningnat * m2r; 


S += (-S_null) + null_S 
	+ v_s + r_s 
	+ (-s_v) 
	+ (-s_e); 
E += (-E_null) 
        + (-e_i) 
	+ s_e ; 
I += (-I_null) 
        + e_i 
        + (-i_r); 
R += (-R_null)
	- r_s 
	+ i_r ; 
V += (-V_null)
	- v_s 
	+ s_v ; 
m1s += -m1s_0 + null_m1s 
	+ irdm1s 
	+ m1r_m1s + m1v_m1s 
	+ -m1s_m1v 
	+ -m1s_m1e ; 
m1e += (-m1e_0)
	+ irdm1e
	+ (-m1e_m1i) 
	+ m1s_m1e ; 
m1i += (-m1i_0)
	+ irdm1i 
	+ m1e_m1i - m1i_m1r;
m1r += (-m1r_0)
	+ irdm1r 
	+ (-m1r_m1s) 
	+ m1i_m1r ; 
m1v += (-m1v_0)
	+ irdm1v
	+ (-m1v_m1s) 
	+ m1s_m1v ; 
	
m2s += (-m2s_0) + null_m2s 
	+ irdm2s 
	+ m2r_m2s + m2v_m2s 
	+ (-m2s_m2v) 
	+ (-m2s_m2e); 
m2e += (-m2e_0)
	+ irdm2e
	+ (-m2e_m2i) 
	+ m2s_m2e ;
m2i += (-m2i_0)
	+ irdm2i 
	+ m2e_m2i - m2i_m2r;
m2r += (-m2r_0)
	+ irdm2r 
	+ (-m2r_m2s) 
	+ m2i_m2r ; 
m2v += (-m2v_0)
	+ irdm2v
	+ (-m2v_m2s) 
	+ m2s_m2v;  
	
Phi += newimports; 
Phi *= exp(-iota2*dt); 
imports += newimports ; 



m2u += (-m2u_0) 
	+ (m2v_m2s) 
	+ m2s_m2u_nme 
	- m2u_m2e ; 

m2w += (-m2w_0) 
	+ (m2r_m2s) 
	- m2w_m2e ; 


Uagg += (e_i*N*pu); 
Wagg += (e_i*N*pw); 
Iagg += (e_i*N);


S = (S>0) ? S : 0.0; 
E = (E>0) ? E : 0.0; 
I = (I>0) ? I : 0.0; 
R = (R>0) ? R : 0.0; 
V = (V>0) ? V : 0.0; 

Iagg = (Iagg>0) ? Iagg : 0.0; 

m1s = (m1s>0) ? m1s : 0.0; 
m1e = (m1e>0) ? m1e : 0.0; 
m1i = (m1i>0) ? m1i : 0.0; 
m1r = (m1r>0) ? m1r : 0.0; 
m1v = (m1v>0) ? m1v : 0.0; 

m2s = (m2s>0) ? m2s : 0.0; 
m2e = (m2e>0) ? m2e : 0.0; 
m2i = (m2i>0) ? m2i : 0.0; 
m2r = (m2r>0) ? m2r : 0.0; 
m2v = (m2v>0) ? m2v : 0.0; 

Phi = (Phi>0) ? Phi : 0.0; 
imports = (imports>0) ? imports : 0.0; 

