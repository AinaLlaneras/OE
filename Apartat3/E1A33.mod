## parametres
param nB; # num busos
param refB; # bus referencia
set B := 1..nB; # busos
set G; # generadors
set D; # unitats de càrrega	
set H; # hores 
set L within B cross B; #arcs 

set GB{B}; # generadors a cada bus 
set DB{B}; # unitats de carrega a cada bus 
set H0= 0..card(H);

param cq{G}; # consum de combustible q
param cl{G}; # consum de combustible l
param cb{G}; # consum de combustible b
param csud{G}; # cost d'operació 
param pgmin{G}; # limit inferior generador
param pgmax{G}; # limit superior generador
param rd{G}; # limit inferior de rampa
param ru{G}; # limit superior de rampa
param pg0{G}; # p inicial
param u0{G}; # u inicial 
param pd{D,H}; # demanda de cada unitat de càrrega
param sb; #potència base
param x{L}; # reactància de la línia 
param smax{L}; #capacitat de la línia
param maxalpha := 0.09; #hem hagut de posar una alpha lleugerament superior a l'obtingut perquè sinó el problema era infactible

## variables
var PG {G, H0}; # generacio de potencia de cada unitat i 
var P {L, H};
var Th {B, H }; # angle de voltatge del bus k 
var u {G, H0}  binary; # estat del generador i (1=ences, 0=apagat) 
var v {G, H} binary; # estat d'encesa del generador i (1=s'ha encès)
var w {G, H} binary;# estat d'aturada del generador i (1=si la unitat s apaga)
var alpha {D, H} >=0, <=1;

## funcio objectiu
minimize CostTotal: sum{i in G, t in H} (cq[i]*PG[i,t]^2 + cl[i]*PG[i,t] + cb[i]*u[i,t] + v[i,t]*csud[i] + w[i,t]*csud[i])
					+ sum{i in D, t in H} 188.3*alpha[i,t]*(1.1*pd[i,t]) ;

## restriccions
# Alpha 
s.t. FRC {i in D, t in H}: maxalpha >= alpha[i,t]; 

# Nodal Power Flow Equations
s.t. NPFE { k in B, t in H } :
	sum{(k,l) in L} P[k,l,t] - sum{(l,k) in L} P[l,k,t] = 
	(sum{i in GB[k]:card(GB[k])>0} PG[i,t]) - (sum{j in DB[k]:card(DB[k])>0} (1-alpha[j,t])*(1.1*pd[j,t])); # alpha obtinguda anteriorment 

# Linear power flow equations 
subject to LPFE {(k,l) in L, t in H } : 
	P[k,l,t] = sb*( Th[k,t]- Th[l,t] ) / x[k,l];
	
# Generation limits 
s.t. GLmin {i in G, t in H} : PG[i,t] >= pgmin[i]*u[i,t] ;
s.t. GLmax {i in G, t in H} : PG[i,t] <= pgmax[i]*u[i,t];

# Line Security 
s.t. LSmin {(k,l) in L, t in H}: P[k,l,t] >= -smax[k,l];
s.t. LSmax {(k,l) in L, t in H}: P[k,l,t] <= smax[k,l];

# Reference bus
s.t. BusRef {t in H } : Th[refB,t] = 0;
	
# Ramp constraint
s.t. RampDown {i in G, t in H}: PG[i,t]-PG[i,(t-1)] >= -rd[i];
s.t. RampUp {i in G, t in H}:   PG[i,t]-PG[i,(t-1)] <= ru[i];

# Initialization
s.t. InitPG0 {i in G}: PG[i,0] = pg0[i];
s.t. InitU0  {i in G}: u[i,0] = u0[i];

# Restriccions de temps 
s.t. rest7 {i in G, t in H}: u[i,t] - u[i, (t-1)] = v[i,t] - w[i,t];
s.t. rest8 {i in G, t in 5..card(H)}: sum{k in (t-4+1)..t} v[i,k] <= u[i,t];
s.t. rest9 {i in G, t in 6..card(H)}: sum{k in (t-5+1)..t} w[i,k] <= 1-u[i,t];