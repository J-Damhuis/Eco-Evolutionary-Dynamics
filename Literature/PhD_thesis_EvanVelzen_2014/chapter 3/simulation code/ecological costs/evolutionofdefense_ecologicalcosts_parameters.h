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
const double a_intra = 1;			//intraspecific competition coefficient

const double updaterate = 0.01;			//smaller timestep-within-timestep to approximate continuous time
const double maxvalue_toxicity = 1;		//maximum trait value of defense by toxicity
const double maxvalue_efficiency = 1;		//maximum trait value of defense by reducing efficiency

//parameters read from input file

//indices that determine which version of the simulation is used
int competitionindex;		//0 = logistic growth / Lotka-Volterra competition; 1 = nutrient competition
int herbivoredynamicsindex;	//0 = generalist herbivores; 1 = specialist herbivores

//other parameters
int numlineages;		//number of plant lineages
int numgenerations;		//number of generations
int numrounds;			//number of replicate simulation runs

double initialvalue_amount;	//initial value of defense by reducing consumption
double initialvalue_toxicity;	//initial value of defense by toxicity

double cost_direct_amount;	//costliness to growth rate of defense by reducing consumption
double cost_direct_toxicity;	//costliness to growth rate of defense by toxicity
double cost_ecological_amount;	//costliness to competitiveness of defense by reducing consumption
double cost_ecological_toxicity;//costliness to competitiveness of defense by toxicity
double effect_amount;		//effect size of defense by reducing consumption
double effect_toxicity;		//effect size of defense by toxicity

double mutationrate_amount;	//mutation rate of defense by reducing consumption
double mutationrate_toxicity;	//mutation rate of defense by toxicity
double totalmutationrate;	//total mutation rate (sum of all three)
double mutationstep;		//mutation step size (standard deviation of normal distribution)
double maxvalue_amount;		//maximum value for defense by reducing competition

double K;			//carrying capacity / total nutrients in system
double growth_constant;		//half-saturation constant for nutrient competition
double basic_growthrate;	//growthrate for defense = 0 or cost = 0
double basic_deathrate;		//death rate

//variables
double free_nutrients;		//free nutrients in the system, in case of nutrient competition
double herbivorebiomass;	//current herbivore biomass

//plant member variables
struct plant{
	double investment_amount;	//level of investment into defense by reducing consumption
	double investment_toxicity;	//level of investment into defense by toxicity
	double growthrate;		//growth rate (dependent on defense)
	double biomass;			//biomass
	double quality;			//quality to herbivores
	double deathrate;		//death rate
	double competitiveness;		//competitiveness	
};

#endif
