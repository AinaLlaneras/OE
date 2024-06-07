
#Parameters and Set definition
set G;											#Generation units
set D;											#Demand units
set bG;											#Number generation blocks
set bD;											#Number of demand blocks
param nB;										#Number of busses
set B = 1..nB;									#Set of busses
set refB within B;								#Reference node
set L within B cross B;							#Transmision lines between busses
set GB {B} default {};	 						#Genertion units per bus
set DB {B} default {};							#Demand units per bus

param pbG {G,bG};			 					#Power bid per geneartion unit, per block B at time T (MW/h)
param lbG {G,bG};								#Price bid per generation unit, per block B at time T (€/MWh)
param pbD {D,bD};								#Power bid per demand unit, per block B at time T (MW/h)
param lbD {D,bD};								#Price bid per demand unit, per block B at time T (€/MWh)
param pgmin{ G };        						# Minimum power output (MW)
param pgmax{ G };        						# Minimum power output (MW)
param ru {G};			 						# Upward ramp (MW/h)
param rd {G};			 						# Downward ramp (MW/h)
param pg0 {G};			 						# Initital power output (MW)
param u0 {G};			 						# Initital state
param x{ L };        							# Line reactance (p.u.)
param smax{ L };        						# Line capacity (MW)
param sb;        								# Base power (MVAr)

#variables definition
var PG {G,bG} >= 0;							    #Matched energy per generator unit, per block B and time T (MW/h)
var PG_total {G union {0}} >= 0;				#Total matched genertion per unit and time
var U {G} binary;								#On/off state generation units
var PD {D,bD} >= 0;							    #Matched energy demand per unit, per block B and time T (MW/h)
var PD_total {D union {0}} >= 0;				#Total matched demand per unit and time T
var P {(k,l) in L} >= -smax[k,l],<=smax[k,l];	#Power flow line ( i,j), time period t
var theta {B};								    #Voltage angle bus k, time period T

#objective function
maximize SW : 
	sum {i in D, k in bD} (lbD[i,k]*PD[i,k]) 
	- sum {i in G, k in bG} (lbG[i,k]*PG[i,k]);

#Nodal market equilibrium
s.t. NME{k in B}:
sum {(k,l) in L} P[k,l]-sum {(l,k) in L}P[l,k] = sum {i in GB[k]}PG_total[i] - sum {i in DB[k]}PD_total[i];

# Line power flow equations
s.t. LPF {(k,l) in L}: P[k,l] = sb*(theta[k]-theta[l])/x[k,l];

#Matched generation
s.t. matched_gen {i in G, k in bG} : PG[i,k] <= pbG[i,k];

#Matched demand
s.t. matched_dem {i in D, k in bD} : PD[i,k] <= pbD[i,k];

#Total matched generation
s.t. tot_matched_gen {i in G} : PG_total[i] = sum {k in bG} PG[i,k];

#Total matched demand
s.t. tot_matched_dem {i in D} : PD_total[i] = sum {k in bD} PD[i,k];

#Ramp limits
s.t. ramp_down {i in G}: PG_total[i] >= -rd[i];
s.t. ramp_up {i in G}: PG_total[i] <= ru[i];

# Generation limits 
s.t. Gmin{i in G}: PG_total[i]>=pgmin[i]*U[i];
s.t. Gmax{i in G}: PG_total[i]<=pgmax[i]*U[i];

# Initialization
s.t. RefB { k  in refB } : theta[k] = 0.0;
