//---------------------------------------------------------------------------
#pragma hdrstop
#include <vector>
#include <fstream>
#include <time.h>
#include <math.h>
#include <stdlib.h>
#include "random.h"
#include "specialization_parameters.h"
#include "specialization.h"
using namespace std;

//---------------------------------------------------------------------------
void getparameters(ifstream &infile) //read parameter values from input file
{
	char *pEnd;
	string stuff;
	int line = 0;
	while(!infile.eof())
	{
		getline(infile, stuff);
              if (line == 0)
			n = strtod(stuff.c_str(), &pEnd);  
		if (line == 1)
			number_timesteps = strtol(stuff.c_str(), &pEnd, 10);
		if (line == 2)
			number_replicas = strtol(stuff.c_str(), &pEnd, 10);
		if (line == 3)
			growthrate = strtod(stuff.c_str(), &pEnd) * 0.5;
		if (line == 4)
			mutationrate = strtod(stuff.c_str(), &pEnd);
		if (line == 5)
			version = strtol(stuff.c_str(), &pEnd, 10);
		if (line == 6)
			a1 = strtod(stuff.c_str(), &pEnd);
		if (line == 7)
			a2 = strtod(stuff.c_str(), &pEnd);
		if (line == 8)
			growth_constant = strtod(stuff.c_str(), &pEnd);
		if (line == 9)
			total_nutrients = strtod(stuff.c_str(), &pEnd);
		line++;
        }
}
//---------------------------------------------------------------------------

#pragma argsused
int main(int argc, char* argv[]) //main program
{
	SetSeed(time(NULL));	
	
	//read parameter values from input file
	string infilename = argv[1];
	ifstream infile;
	do
		infile.open (infilename.c_str());
	while (infile.is_open() == false);
	getparameters(infile);
	infile.close();

	//make output files, write some of the parameters to output files
	string filename1 = argv[2];
	string filename2 = argv[3];
	string filename3 = argv[4];
	ofstream outfile (filename1.c_str());
	ofstream outfile2 (filename2.c_str());
	ofstream outfile3 (filename3.c_str());
	outfile2 << number_replicas << "\t" << n << "\t" << number_timesteps / 100 << "\t" << mutationrate << "\t" << total_nutrients / 1000 << endl;
	outfile3 << number_replicas << "\t" << number_plantspecies << "\t" << mutationrate << "\t" << total_nutrients / 1000 << endl;

	//output vectors for trait distribution time series, abundance time series, and individual values at the end of each simulation run
	vector< vector<double> > outputdistribution ((number_plantspecies + 1) * number_replicas + 2, vector<double> (100 * numoutputcategories, 0));
	vector< vector<double> > outputabundance ((number_plantspecies + 2) * number_replicas + 1, vector<double> (number_timesteps / 10, 0));
	vector< vector<double> > outputindvalues(number_herbivores, vector<double> ((number_plantspecies + 3) * number_replicas, 0));
	
	for (int num_r = 0; num_r < number_replicas; num_r++) //number of replicate simulation runs
	{
		//initialize simulation run: create vectors of plants and herbivores and set them to initial values, and calculate amount of free nutrients in the system
		vector<double> plants(number_plantspecies);
		vector<animal> herbivores(number_herbivores);
		initializeanimals(herbivores);
		for (int i = 0; i < number_plantspecies; i++)
			plants[i] = initial_plantbiomass;
		double free_nutrients = total_nutrients - conversionfactor * initial_plantbiomass * quality * number_plantspecies - conversionfactor * initial_herbivorebiomass;
		
		//vector of indices of herbivores, used to randomize the order in which herbivores are attacking plants in each time step
		vector<int> consumers(herbivores.size());
		initializeconsumers(consumers);

		int outputrate = number_timesteps / 100; //resolution for trait distribution timeseries

		for (int i = 0; i < number_timesteps; i++) //the actual simulation
		{
			if ((i+1) % 10 == 0) //record abundances of plants, herbivores and nutrients every 10 time units
				generateabundanceseries(outputabundance, plants, herbivores, free_nutrients, (i + 1) / 10, num_r);

			for (double j = 0; j < 1; j += updaterate) //divide each timestep in number of small steps (20-50) to approximate continuous time for the ecological and evolutionary dynamics
			{                                        
				shuffle(consumers); 								//randomize order in which herbivores consume plants
				ecologicaldynamics(plants, herbivores, consumers, free_nutrients); 	//ecological interactions: herbivore consumption of plants
				mutation(herbivores, free_nutrients, updaterate); 			//evolution: herbivore mutation
			}
			if ((i + 1) % outputrate == 0) //record trait distribution 100 times throughout simulation run to make distribution plots
				output_timeseriessp(plants, herbivores, outputdistribution, (i + 1) / outputrate, outputrate, num_r);
			if (i == number_timesteps - 1) //at the end of each simulation run, record all individual trait values and biomasses
				generateindividualvalues(outputindvalues, herbivores, num_r);
		}
	}

	//write results of all simulation runs to files
	output_abundanceseries (outfile, outputabundance);
	output_indvalues (outfile3, outputindvalues);
	output_sp (outfile2, outputdistribution);
	outfile.close();
	outfile2.close();
	outfile3.close();
	return 0;
}
//---------------------------------------------------------------------------
