//---------------------------------------------------------------------------

#include <vector>
#include <math.h>
#include <fstream>
#include <stdlib.h>
#include "random.h"
#include "evolutionofdefense_uptakedeath_parameters.h"
#include "evolutionofdefense_uptakedeath_indvalues.h"
#pragma hdrstop

using namespace std;
//---------------------------------------------------------------------------
void getparameters(ifstream &infile)
{
	//read parameter values from input file
	char *pEnd;
	string stuff;
	int line = 0;
	while (!infile.eof())
	{
		getline(infile, stuff);
		if (line == 0)
			numgenerations = strtol(stuff.c_str(), &pEnd, 10);
		if (line == 1)
			numrounds = strtol(stuff.c_str(), &pEnd, 10);
		if (line == 2)
			numlineages = strtol(stuff.c_str(), &pEnd, 10);
		if (line == 3)
			effect_amount = strtod(stuff.c_str(), &pEnd);
		if (line == 4)
			mutationrate_amount = strtod(stuff.c_str(), &pEnd);
		if (line == 5)
			mutationstep = strtod(stuff.c_str(), &pEnd);
		if (line == 6)
			initialvalue_amount = strtod(stuff.c_str(), &pEnd);
		if (line == 7)
			cost_uptake = strtod(stuff.c_str(), &pEnd);
		if (line == 8)
			cost_death = strtod(stuff.c_str(), &pEnd);
		if (line == 9)
			maxvalue_amount = strtod(stuff.c_str(), &pEnd);
		if (line == 10)
			basic_growthrate = strtod(stuff.c_str(), &pEnd);
		if (line == 11)
			basic_deathrate = strtod(stuff.c_str(), &pEnd);
		if (line == 12)
			K = strtod(stuff.c_str(), &pEnd);
		if (line == 13)
			growth_constant = strtod(stuff.c_str(), &pEnd);
		if (line == 14)
			competitionindex = strtol(stuff.c_str(), &pEnd, 10);
		if (line == 15)
			herbivoredynamicsindex = strtol(stuff.c_str(), &pEnd, 10);
		line++;
	}
}
//---------------------------------------------------------------------------
#pragma argsused
int main(int argc, char* argv[])
{
	//read parameter values from input file
	string infilename = argv[1];
	ifstream infile;
	do
		infile.open(infilename.c_str());
	while (infile.is_open() == false);
	getparameters(infile);
	infile.close();
	
	//create output files, write seed of random number generator to output files
	string outfilename1 = argv[2];
	string outfilename2 = argv[3];
	ofstream outfile (outfilename1.c_str());
	ofstream outfile2(outfilename2.c_str());
	int seed = atoi(argv[4]);
	SetSeed(seed);
	outfile << seed << endl;
	outfile2 << seed << endl;
	
	int resolution = numgenerations / 100;
	
	//create array of plant lineages and array of indices to allow order to be randomized in the simulation
	vector<plant> plants (numlineages);
	vector<int> plant_order(numlineages);
	
	//arrays in which results are stored, to write to file after all simulation runs are done
	vector< vector<double> > outputdistribution (2 + numrounds, vector<double> (5000, 0));
	vector< vector<double> > outputindvalues(2 * numrounds, vector<double> (numlineages, 0));
	
	for (int i = 0; i < numrounds; i++)
	{
		//initialize simulation
		initialize(plants);
		initialize_consumption(plant_order);
		herbivorebiomass = basic_herbivorebiomass;
		if (herbivoredynamicsindex == 0) //if generalist herbivores, herbivore biomass is disregarded in the nutrient pool
			free_nutrients = K - conversionfactor * basic_quality * initial_biomass;
		else free_nutrients = K - conversionfactor * basic_quality * initial_biomass - conversionfactor * herbivorebiomass;
		
		//actual simulation
		for (int j = 0; j < numgenerations; j++)
 		{
			for (double k = 0; k < 1; k += updaterate) //each timestep divided into smaller steps to approximate continuous time
			{
				plantgrowth(plants);
				consumption(plants, plant_order);
				mutation(plants);
			}
			shuffle(plant_order); //re-shuffle order in which they are consumed each timestep
			if ((j + 1) % resolution == 0)
				generateoutput(plants, outputdistribution, i, (j + 1) / resolution, resolution); //store distribution of trait values
		}
		generateindvalues(plants, outputindvalues, i); //store individual values at the end of each simulation run
	}
	writeoutput(outputdistribution, outputindvalues, outfile, outfile2); //write results to file
        
	return 0;
}
//---------------------------------------------------------------------------
