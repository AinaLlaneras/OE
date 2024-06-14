
#Parameters and Set definition
set G;											# Generation units
set D;											# Demand units
set bG;											# Number generation blocks
set bD;	
set B;											# Number of demand blocks
set L within B cross B;							# Transmision lines between busses
set GB {B} default {};	 						# Genertion units per bus
set DB {B} default {};							# Demand units per bus
set T;											

param pbG {G,bG,T};			 					# Power bid per geneartion unit, per block B at time T (MW/h)
param lbG {G,bG,T};								# Price bid per generation unit, per block B at time T (�/MWh)
param pbD {D,bD,T};								# Power bid per demand unit, per block B at time T (MW/h)
param lbD {D,bD,T};								# Price bid per demand unit, per block B at time T (�/MWh)
param pgmin{ G };        						# Minimum power output (MW)
param pgmax{ G };        						# Minimum power output (MW)
param ru {G};			 						# Upward ramp (MW/h)
param rd {G};			 						# Downward ramp (MW/h)
param pg0 {G};			 						# Initital power output (MW)
param u0 {G};			 						# Initital state
param x{ L };        							# Line reactance (p.u.)
param smax{ L };        						# Line capacity (MW)
param sb;        								# Base power (MVAr)

var PG {G,bG, T} >= 0;							# Matched energy per generator unit, per block B and time T (MW/h)
var PG_total {G union {0}, T union {0}} >= 0;	# Total matched genertion per unit and time
var U {G,T} binary;								# On/off state generation units
var PD {D,bD,T} >= 0;							# Matched energy demand per unit, per block B and time T (MW/h)
var PD_total {D union {0}, T} >= 0;				# Total matched demand per unit and time T
var P {(k,l) in L, T} >= -smax[k,l],<=smax[k,l];# Power flow line ( i,j), time period t
var theta {B, T};								# Voltage angle bus k, time period T

# objective function

maximize SW: 
	sum {t in T}(sum {i in D, k in bD} (lbD[i,k,t]*PD[i,k,t]) - sum {i in G, k in bG} (lbG[i,k,t]*PG[i,k,t]));

# Nodal market equilibrium # aqui tenim una restriccio general pero es pot descomposar en tres 
s.t. NME {k in B, t in T} :
	sum {(k,l) in L} P[k,l,t] - sum{(l,k) in L} P[l,k,t] =  # tota la potencia que surt del bus menys la que entra ha de ser igual a la potencia que genera la unitat menys la que demanda
	sum {i in GB[k]} PG_total[i,t] - sum{i in DB[k]}PD_total[i,t];

# Total matched generation
s.t. tot_matched_gen {i in G, t in T} : PG_total[i,t] = sum{k in bG} PG[i,k,t];

# Total matched demand
s.t. tot_matched_dem {i in D, t in T} : PD_total[i,t] = sum{k in bD} PD[i,k,t];

# Matched generation: 
s.t. matched_gen {k in bG, i in G, t in T}: PG[i,k,t] <= pbG[i,k,t];

# Matched demand
s.t. matched_dem {k in bD, i in D, t in T}: PD[i,k,t] <= pbD[i,k,t];

# Line power flow equations
s.t. LPF {(k,l) in L, t in T}: 
	P[k,l,t] = sb*(theta[k,t] - theta[l,t])/x[k,l];
	
# Theta reference
s.t. RefB {t in T} : theta[1,t] = 0.0;

# Ramp limits
s.t. ramp_down {i in G, t in T}:  PG_total[i,t]-PG_total[i, (t-1)] >= -rd[i];
s.t. ramp_up {i in G, t in T}:  PG_total[i,t]-PG_total[i, (t-1)] <= ru[i];

# Generation limits 
s.t. Gmin {i in G, t in T}: PG_total[i,t] >= pgmin[i]*U[i,t];
s.t. Gmax {i in G, t in T}: PG_total[i,t] <= pgmax[i]*U[i,t];
