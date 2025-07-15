/*
"S"
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
, "m2u" # susceptible previous vac 
, "m2w" # susceptible previous inf 
, "imports"
, "Phi" # Exp weighted MA imports 
, "logrscale" # r random walk ic params 

I_0 
*/

double M = 1.0; 
double popM2 = pow(1/alpha,2) * alpha * (alpha + 1 ); 

S = 1.0-initv*ve1; 
E = 0.0;
I = 0.0;
R = 0.0; 

V = 0.0; 

Iagg = 0.0; 
Uagg = 0.0; 
Wagg = 0.0; 

m1s = 1.0-initv*ve1;
m1e = 0.0; 
m1i = 0.0; 
m1r = 0.0; 

m1v = 0.0; 

m2s = popM2*(1.0-initv*ve1); 
m2e = 0.0; 
m2i = 0.0;
m2r = 0.0; 

m2v = 0.0; 

m2u = initv*(1-ve1); 
m2w = 0.0; 

imports = 0.0; 
Phi = 0.0; 

logrscale = 0.0;

