## Parametres
param nB; # num busos
param refB; # bus referencia

set B := 1..nB; # busos
set G; # generadors
set D; # unitats de càrrega	
set H; # hores 
set L within B cross B; # arcs 
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
param sb; # potència base
param x{L}; # reactància de la línia 
param smax{L}; #capacitat de la línia
 

## Variables
var PG {G, H0}; # generacio de potencia de cada generador i 
var P {L, H}; # p
var Th {B, H }; # angle de voltatge del bus k 
var u {G, H0}  binary; # estat del generador i (1=ences, 0=apagat) 
var vud {G, H} >=0; # cost d'encesa (>=0)

## Funcio objectiu: minimitzem costos
minimize CostTotal : sum{ i in G, t in H } 
	(cq[i]*PG[i,t]^2+cl[i]*PG[i,t]+cb[i]*u[i,t] + vud[i,t]);

## Restriccions 
# Nodal Power Flow Equations
s.t. NPFE { k in B, t in H } :
	sum{(k,l) in L} P[k,l,t] - sum{(l,k) in L} P[l,k,t] = 
	(sum{i in GB[k]: card(GB[k])>0} PG[i,t]) - (sum{j in DB[k]: card(DB[k])>0} pd[j,t]);

# Linear power flow equations 
subject to LPFE {(k,l) in L, t in H } : 
	P[k,l,t] = sb*( Th[k,t]- Th[l,t] ) / x[k,l];
	
# Generation limits 
s.t. GLmin {i in G, t in H}: PG[i,t] >= pgmin[i]*u[i,t] ;
s.t. GLmax {i in G, t in H}: PG[i,t] <= pgmax[i]*u[i,t];

# Line Security 
s.t. LSmin {(k,l) in L, t in H}: P[k,l,t] >= -smax[k,l];
s.t. LSmax {(k,l) in L, t in H}: P[k,l,t] <= smax[k,l];

# Reference bus
s.t. BusRef {t in H } : Th[refB,t] = 0;
	
# Ramp constraint
s.t. RampDown {i in G, t in H}: PG[i,t]-PG[i,(t-1)] >= -rd[i];
s.t. RampUp {i in G, t in H}:   PG[i,t]-PG[i,(t-1)] <= ru[i];

# Sart up cost 
s.t. StartCost {i in G, t in H}: vud[i,t] >= csud[i]*(u[i,t] - u[i, (t-1)]);

# Shut down cost 
s.t. ShutCost {i in G, t in H}: vud[i,t] >= csud[i]*(u[i,(t-1)] - u[i, t]);
	
# Initialization
s.t. InitPG0 {i in G}: PG[i,0] = pg0[i];
s.t. InitU0 {i in G}: u[i,0] = u0[i];
 