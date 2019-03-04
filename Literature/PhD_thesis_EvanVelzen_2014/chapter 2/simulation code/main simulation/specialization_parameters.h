#ifndef SPECIALIZATION_PARAMS_H
#define SPECIALIZATION_PARAMS_H

//parameters that are kept constant

const double attackrate = 0.00001;		//herbivore attack rate
const double handlingtime = 0.1;		//herbivore handling time
const double deathrate_plants = 0.05;	//natural mortality of plants
const double deathrate_animals = 0.4;	//natural mortality of herbivores
const double mutationstep = 0.01;		//mutation step size

const double conversionfactor = 2;		//conversion factor of nutrients to plant / animal biomass
const double quality = 0.5;			//extra conversion factor for plants
const double conversion_efficiency = 0.25;//conversion efficiency; efficiency with which plant biomass is turned into animal biomass

const double updaterate = 0.05;		//to divide up each time unit into smaller steps
const int numoutputcategories = 50;	//for trait distribution plots

const int number_plantspecies = 2;			//number of plant species
const double initial_plantbiomass = 500000;	//initial plant biomass
const double initial_herbivorebiomass = 100000;	//initial herbivore biomass
const int number_herbivores = 400;			//number of herbivore lineages
const double deviation = 0.1;			//standard deviation of initial distribution of herbivore preference trait (mean = 0.5)


//parameters that are read from input file

int number_replicas;		// number of simulation runs
int number_timesteps;	// timesteps per simulation run
double n;			// trade-off coefficient
double total_nutrients;	// total nutrients in the system
double mutationrate;		// probability of mutation per lineage per time unit
double growthrate; 		// intrinsic plant growth rate; is actually "quality * growthrate" due to the way it's implemented in the growth function
int version; 			// 1 = ratio dependent, 0 = "standard" version of the model

// for ratio dependent version
double a1;			//intraspecific competition coefficient
double a2;			//interspecific competition coefficient
double growth_constant;	//growth constant, see growth function

struct animal	//member variables of herbivore
{
        double preference[10];  //simulation can go up to 10 plant species; preference for each
        double totalpreference; //used to normalize so that total preference for all plant species will sum up to 1
        double biomass;	    //biomass
        double specialization;  //index of specialization; measure of deviation from "equal preference for all species", where 0 is complete generalist and 1 is complete specialist
        int speciesnumber;      //used to generate more readable version of individual values; no effect on simulation itself
};

#endif
