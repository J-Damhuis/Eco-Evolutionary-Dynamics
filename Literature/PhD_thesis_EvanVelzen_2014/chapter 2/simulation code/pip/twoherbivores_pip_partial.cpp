//---------------------------------------------------------------------------

#pragma hdrstop
#include <stdlib.h>
#include <math.h>
#include <fstream>
#include <vector>
using namespace std;

//parameter values kept constant
const double quality = 0.5;
const double deathrate_plants = 0.05;

const double attackrate = 0.00001;
const double handlingtime = 0.1;
const double conversionfactor = 2;
const double conversionefficiency = 0.25;
const double deathrate_animals = 0.4;

//parameter values read from input file
double updaterate;		//smaller timesteps-within-timesteps to approximate continuous time
int numgenerations;		//number of timesteps before the mutants are introduced
double resolution;		//distance between trait values of mutants (always used 0.005)
double growthrate;		//intrinsic growth rate of plants
double totalnutrients;	//total nutrients in the system

double x_begin;		//begin and end values of resident generalists
double x_end;
double n;			//trade-off coefficient
double calculatesteps;	//number of timesteps used to calculate mutant fitness

//plants, herbivores, nutrients
double free_nutrients;
vector<double> plantbiomass (2);
vector<double> animalbiomass (2);
vector<double> animalpreference (2);

//---------------------------------------------------------------------------
void getparameters(ifstream &infile)
{
	//read parameters from input file
	char *pEnd;
	string stuff;
	int line = 0;
	while(!infile.eof())
	{
		getline(infile, stuff);
		if (line == 0)
			n = strtod(stuff.c_str(), &pEnd);
		if (line == 1)
			growthrate = strtod(stuff.c_str(), &pEnd);
		if (line == 2)
			totalnutrients = strtod(stuff.c_str(), &pEnd);
		if (line == 3)
			numgenerations = strtol(stuff.c_str(), &pEnd, 10);
		if (line == 4)
			updaterate = strtod(stuff.c_str(), &pEnd);
		if (line == 5)
			resolution = strtod(stuff.c_str(), &pEnd);
		if (line == 6)
			calculatesteps = strtod(stuff.c_str(), &pEnd);
		if (line == 7)
			x_begin = strtod(stuff.c_str(), &pEnd);
		if (line == 8)
			x_end = strtod(stuff.c_str(), &pEnd);
		line++;
	}
}
//---------------------------------------------------------------------------
void plantgrowth()
{
	//plant growth: first determine for each plant species how much growth / death there is, then actually add / subtract from current biomass
	vector<double> growthrates(2);
	for (int i = 0; i < 2; i++)
		growthrates[i] = updaterate * plantbiomass[i] * (growthrate * free_nutrients / (free_nutrients + totalnutrients) - deathrate_plants * conversionfactor);
	for (int i = 0; i < 2; i++)
	{
		plantbiomass[i] += growthrates[i] / conversionfactor;
		free_nutrients -= growthrates[i] * quality;
	}
}
//---------------------------------------------------------------------------
void consumption()
{
	//herbivore mortality
	for (int i = 0; i < 2; i++)
	{
		free_nutrients += updaterate * deathrate_animals * animalbiomass[i] * conversionfactor;
		animalbiomass[i] -= updaterate * deathrate_animals * animalbiomass[i];
	}

	//consumption of plants by herbivores. Again, first determine how much consumption of each plant species by each herbivore, then do the actual dynamics
	vector<double> consumptionrates1(2);
	vector<double> consumptionrates2(2);
	for (int i = 0; i < 2; i++)
	{
		consumptionrates1[i] = updaterate * animalbiomass[i] * attackrate * (plantbiomass[0] * pow(animalpreference[i], n)) / (1 + handlingtime * attackrate * (plantbiomass[0] * pow(animalpreference[i], n) + plantbiomass[1] * pow(1 - animalpreference[i], n)));
		consumptionrates2[i] = updaterate * animalbiomass[i] * attackrate * (plantbiomass[1] * pow(1 - animalpreference[i], n)) / (1 + handlingtime * attackrate * (plantbiomass[0] * pow(animalpreference[i], n) + plantbiomass[1] * pow(1 - animalpreference[i], n)));
	}
	for (int i = 0; i < 2; i++)
	{
		plantbiomass[0] -= consumptionrates1[i];
		plantbiomass[1] -= consumptionrates2[i];
		animalbiomass[i] += conversionefficiency * (consumptionrates1[i] + consumptionrates2[i]) * quality;
		free_nutrients += (1 - conversionefficiency) * (consumptionrates1[i] + consumptionrates2[i]) * conversionfactor * quality;
	}
}
//---------------------------------------------------------------------------
double testconsumption(double x)
{
	//total consumption of both plant species by a mutant with preference "x"
	double stuff = attackrate * (plantbiomass[0] * pow(x, n) + plantbiomass[1] * pow(1 - x, n)) / (1 + handlingtime * attackrate * (plantbiomass[0] * pow(x, n) + plantbiomass[1] * pow(1 - x, n)));
	return stuff;
}
//---------------------------------------------------------------------------
#pragma argsused
int main(int argc, char* argv[])
{
	//get parameter values from input file
	string infilename = argv[1];
	ifstream infile;
	do
		infile.open (infilename.c_str());
	while (infile.is_open() == false);
	getparameters(infile);
	infile.close();
	
	//create output file; write mutant trait values as headers
	string outfilename = argv[2];
	ofstream outfile1 (outfilename.c_str());
	outfile1 << "\t";
	for (double i = 0.5; i <= 1.00001; i+= resolution)
		outfile1 << i << "\t";
	outfile1 << endl;

	//simulate ecological dynamics in a resident population of 1 specialist (x = 0) and 1 generalist (x between 0.5 and 1)
	//determine fitness of a rare mutant with trait value ranging from 0.5 - 1 thrown into this population
	for (double x = x_begin; x <= x_end + 0.0001; x += resolution)
	{
		//set up initial values of simulation
		animalpreference[0] = 0;
		animalpreference[1] = x;
		for (int i = 0; i < 2; i++)
		{
			plantbiomass[i] = 500000;
			animalbiomass[i] = 5000;
		}
		free_nutrients = totalnutrients - 2 * 500000 - 2 * 2 * 5000;

		//first allow simulation to run for several thousands of timesteps with just residents, so ecological dynamics reach their equilibrium state
		for (int i = 0; i < numgenerations / updaterate; i++)
		{
			plantgrowth();
			consumption();
		}

		//set up array of mutant fitness values, set to zero
		vector<double> totalfitness(int(1 / resolution) / 2 + 1);
		for (int i = 0; i < totalfitness.size(); i++)
			totalfitness[i] = 0;

		//continue simulating ecological dynamics, and keep track of how much a mutant with each preference value would consume
		//add those up for a fixed number of timesteps (several thousand, to reduce the effect of the oscillations)
		//mutants have no effect on ecological dynamics
		for (int i = 0; i < calculatesteps / updaterate; i++)
		{
			plantgrowth();
			consumption();
			for (int z = 0; z < totalfitness.size(); z++)
				totalfitness[z] += updaterate * testconsumption(double(z) * resolution + 0.5);
		}

		//fitness of the mutant with same trait value as the resident is set to 1, the rest are normalized to this
		//write resident preference value "x" and mutant fitness values to file
		double averagefitness = totalfitness[int((x + 0.00001 - 0.5) / resolution)];
		outfile1 << x << "\t";
		for (int i = 0; i < totalfitness.size(); i++)
		{
			totalfitness[i] /= averagefitness;
			outfile1 << totalfitness[i] << "\t";
		}
		outfile1 << endl;
	}
	return 0;
}
//---------------------------------------------------------------------------
