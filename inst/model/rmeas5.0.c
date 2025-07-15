
casesbreakthrough = rbinom( nearbyint(Iagg), sampf*Uagg/(Iagg+1e-6)); 
casesreinf = rbinom( nearbyint( Iagg ), sampf*Wagg/(Iagg+1e-6));
cases = casesbreakthrough + casesreinf + rbinom( nearbyint(Iagg), sampf*(Iagg - Uagg - Wagg)/(Iagg+1e-6)); 


