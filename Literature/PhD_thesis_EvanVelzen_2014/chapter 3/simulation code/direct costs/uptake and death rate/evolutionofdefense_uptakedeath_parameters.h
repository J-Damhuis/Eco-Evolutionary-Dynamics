#ifndef PLANTEVOLUTION_PARAMETERS_COST2_H
#define PLANTEVOLUTION_PARAMETERS_COST2_H

//parameters kept constant in the simulation
const double initial_biomass = 500000;		//initial plant biomass
const double basic_herbivorebiomass = 10000;	//initial herbivore biomass / constant herbivore biomass in case of generalist herbivores
const double basic_quality = 0.5;		//how many units of nutrients are in 1 unit of plant biomass
const double deathrate_animals = 0.4;		//mortality of herbivores
const double conversionfactor = 2.0;		//conversion factor between nutrients and plant / herbivore biomass
const double conversion_efficiency = 0.25;	//conversion efficiency of consumed plant biomass into herbivore biomass
const double attackrate = 0.00001;		//herbivore attack rate
const double basic_handlingtime = 0.1;		//herbivore handling time

const double updaterate = 0.01;			//smaller timestep-within-timestep to approximate continuous time

//parameters read from input file

//indices that determine which version of the simulation is used
int competitionindex;		//0 = logistic growth / Lotka-Volterra competition; 1 = nutrient competition
int herbivoredynamicsindex;	//0 = generalist herbivores; 1 = specialist herbivores

//other parameters
int numlineages;	//number of plant lineages
int numgenerations;	//number of generations
int numrounds;		//number of replicate simulation runs

double initialvalue_amount;	//initial value of defense by reducing consumption

double cost_uptake;		//costliness of defense to uptake rate
double cost_death;		//costliness of defense to death rate
double effect_amount;		//effect size of defense by reducing consumption

double mutationrate_amount;	//mutation rate of defense by reducing consumption
double mutationstep;		//mutation step size (standard deviation of normal distribution)
double maxvalue_amount;		//maximum value for defense by reducing competition

double K;			//carrying capacity
double growth_constant;		//half-saturation constant for nutrient limited growth
double basic_growthrate;	//growthrate for defense = 0 or cost = 0
double basic_deathrate;		//death rate

//variables
double free_nutrients;		//free nutrients in the system, in case of nutrient competition
double herbivorebiomass;	//current herbivore biomass

//plant member variables
struct plant{
	double investment_amount;	//level of investment into defense by reducing consumption
	double growthrate;		//growth rate
	double biomass;			//biomass
	double quality;			//quality to herbivores
	double uptakerate;		//uptake rate of nutrients for growth (dependent on defense)
	double deathrate;		//death rate (dependent on defense)
};

#endif
