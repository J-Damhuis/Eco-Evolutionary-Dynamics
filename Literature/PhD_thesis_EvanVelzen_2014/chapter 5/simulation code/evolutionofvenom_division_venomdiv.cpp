//---------------------------------------------------------------------------

#include <vector>
#include <math.h>
#include <fstream>
#include <stdlib.h>
#include "random.h"
#pragma hdrstop

using namespace std;
//---------------------------------------------------------------------------

//member variables of parasitoid
struct parasitoid
{
	double venomousness;	//level of venom production
	double division_v;	//division of venom into parasitized and unparasitized hosts
	double division_f;	//division of eggs into parasitized and unparasitized hosts
	double fecundity1;	//clutch size on unparasitized hosts
	double fecundity2;	//clutch size on parasitized hosts
	double venomousness1;	//amount of venom injected into unparasitized hosts
	double venomousness2;	//amount of venom injected into parasitized hosts
	int numeggs;		//number of eggs laid into any given host (temporary variable only)
	int numoffspring;	//total number of offspring from all parasitizations
};

//member variables of host
struct host
{
	int numencounters;	//number of encounters with parasitoids (drawn from negative binomial distribution)
	bool parasitized;	//0 if unparasitized or no eggs have been laid in it during previous encounters, 1 if it contains eggs
	double totalvenom;	//total amount of venom injected by all parasitoids
	bool surviving;		//1 if total venom injected was not enough to kill it; 0 if it wasn't, it survives to reproduce for the next generation
	int numeggs;		//total number of eggs laid in host
	double eggsurvival;	//survival probability of parasitoids (depending on total eggs laid and carrying capacity)
};

//parameter values read from input file
int numruns;			//number of simulation runs
int numgenerations;		//number of generations
double attackrate;		//parasitoid attack rate
double maxfecundity;		//max fecundity (when venom production = 0)
double b;			//costliness of venom production
double lambda;			//intrinsic growth rate of hosts
double k;			//host aggregation parameter
double d;			//host death rate
int initialhosts;		//number of hosts at start of simulation
int initialparasitoids;		//number of parasitoids at start of simulation
double initialv;		//initial value for venom production
double initialdivision_v;	//initial value for venom division
double initialdivision_f;	//initial value for clutch division
double maxc;			//within-host carrying capacity
double mutationrate;		//mutation rate (probability per individual per generation)
double mutationstep;		//mutation step (standard deviation of distribution from which new value is drawn)
double k_death;			//half-saturation constant determining effect of venom on host
double n_death;			//constant of nonlinearity for determining effect of venom on host

double pfirst;

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
			numruns = strtol(stuff.c_str(), &pEnd, 10);
		if (line == 1)
			numgenerations = strtol(stuff.c_str(), &pEnd, 10);
		if (line == 2)
			attackrate = strtod(stuff.c_str(), &pEnd);
		if (line == 3)
			maxfecundity = strtod(stuff.c_str(), &pEnd);
		if (line == 4)
			b = strtod(stuff.c_str(), &pEnd);	
		if (line == 5)
			lambda = strtod(stuff.c_str(), &pEnd);
		if (line == 6)
			k = strtod(stuff.c_str(), &pEnd);
		if (line == 7)
			d = strtod(stuff.c_str(), &pEnd); 
		if (line == 8)
			initialhosts = strtol(stuff.c_str(), &pEnd, 10);
		if (line == 9)
			initialparasitoids = strtol(stuff.c_str(), &pEnd, 10);
		if (line == 10)
			initialv = strtod(stuff.c_str(), &pEnd);
		if (line == 11)
			initialdivision_v = strtod(stuff.c_str(), &pEnd);
		if (line == 12)
			initialdivision_f = strtod(stuff.c_str(), &pEnd);
		if (line == 13)
			maxc = strtod(stuff.c_str(), &pEnd);
		if (line == 14)
			mutationrate = strtod(stuff.c_str(), &pEnd);
		if (line == 15)
			mutationstep = strtod(stuff.c_str(), &pEnd);
		if (line == 16)
			k_death = strtod(stuff.c_str(), &pEnd);
		if (line == 17)
			n_death = strtod(stuff.c_str(), &pEnd);	
		line++;
	}
}
//---------------------------------------------------------------------------
void calculatefirst(int psize)
{
	//calculate probability that any given encounter is with an unparasitized host: total number of encounters divided by total hosts encountered
	double averageattack = attackrate * psize;
	double escape = pow((1.0 + averageattack / k), -k);
	pfirst = (1.0 - escape) / averageattack;
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// DRAW NUMBERS FROM POISSON, GAMMA AND NEGATIVE BINOMIAL DISTRIBUTION
//---------------------------------------------------------------------------
int poisson (double average)
{
	//draw number from poisson distribution with mean = average
	double L = exp(-average);
	int j = 0;
	double p = 1.0;
	do{
		j++;
		p *= Uniform();
	}
	while (p > L);
	return j - 1;
}
//---------------------------------------------------------------------------
double checkg1(double p)
{
	double x = pow(p, 1.0 / k);
	if (exp(-x) > Uniform())
		return x;
	return -1;
}
//---------------------------------------------------------------------------
double checkg2(double p, double b)
{
	double x = -log((b - p) / k);
	if (pow(x, k - 1.0) > Uniform())
		return x;
	return -1;
}
//---------------------------------------------------------------------------
double gamma()
{
	//draw number from gamma distribution
	double b = (exp(1.0) + k) / exp(1.0);
	double x;
	do{
		double p = Uniform() * b;
		if (p <= 1.0)
			x = checkg1(p);
		else x = checkg2(p, b);
	}
	while (x < 0);
	return x;
}
//---------------------------------------------------------------------------
int negbinom(double mean)
{
	//draw number from negative binomial distribution with parameters mean, k for mean and aggregation
	double p = 1.0 - mean / (mean + k);
	double pmean = gamma() * ((1.0 - p) / p);
	int x = poisson(pmean);
	return x;
}

//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// INITIALIZE SIMULATION, UPDATE HOSTS AND PARASITOID VARIABLES AT START OF EACH GENERATION
//---------------------------------------------------------------------------
void updatehost(host &x)
{
	//set member variables to initial conditions at start of generation
	x.parasitized = false;	//not parasitized
	x.totalvenom = 0;	//no venom injected
	x.numencounters = 0;	//no encounters
	x.numeggs = 0;		//no eggs laid in it
	x.surviving = true;	//not killed by parasitoids injecting venom
}
//---------------------------------------------------------------------------
void updateparasitoid (parasitoid &x)
{
	//calculate parasitoid traits and reset other variables to initial conditions
	double f = maxfecundity - b * x.venomousness; //fecundity as a function of venom production and costliness of venom

	// clutch size unparasitized and parasitized, venom injected into unparasitized and parasitized
	x.fecundity1 = f * x.division_f / (pfirst * x.division_f + (1.0 - pfirst) * (1.0 - x.division_f));
	x.fecundity2 = f * (1.0 - x.division_f) / (pfirst * x.division_f + (1.0 - pfirst) * (1.0 - x.division_f));
	x.venomousness1 = x.venomousness * x.division_v / (pfirst * x.division_v + (1.0 - pfirst) * (1.0 - x.division_v));
	x.venomousness2 = x.venomousness * (1.0 - x.division_v) / (pfirst * x.division_v + (1.0 - pfirst) * (1.0 - x.division_v));
	
	x.numoffspring = 0;	//no offspring
	x.numeggs = 0;		//no eggs laid
}
//---------------------------------------------------------------------------
void initialize (vector<host> &h, vector<parasitoid> &p)
{
	//initialize hosts and parasitoids at the beginning of simulation
	
	h.resize(initialhosts);		//reset host population size to initial population size
	p.resize(initialparasitoids);	//same for parasitoid population size
	
	calculatefirst(initialparasitoids);	//calculate fraction of encounters that involves unparasitized hosts
	for (int i = 0; i < h.size(); i++)
		updatehost(h[i]);		//reset host variables to initial conditions
	
	for (int i = 0; i < p.size(); i++)
	{
		p[i].venomousness = initialv;		//reset parasitoid inherited traits to initial conditions
		p[i].division_f = initialdivision_f;
		p[i].division_v = initialdivision_v;
		updateparasitoid(p[i]);			//recalculate trait values and reset to initial conditions
	}
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// PARASITIZATION
//---------------------------------------------------------------------------
void distributeencounters(vector<host> &h, vector<parasitoid> &p)
{
	//for each host, draw number of encounters with parasitoids from normal distribution
	double averageencounters = attackrate * p.size();
	for (int i = 0; i < h.size(); i++)
		h[i].numencounters = negbinom(averageencounters);
}
//---------------------------------------------------------------------------
int calculateeggs (double f)
{
	//calculate number of eggs laid during encounter: draw from poisson distribution with mean = clutch size unparasitized / parasitized
	int x = poisson(f);
	return x;
}
//---------------------------------------------------------------------------
int calculateoffspring(int numeggs, double survival)
{
	//calculate number of offspring per parasitoid, depending on number of eggs laid and survival probability per egg
	int x = 0;
	for (int i = 0; i < numeggs; i++)
		if (survival > Uniform())
			x++;
	return x;
}
//---------------------------------------------------------------------------
double calculatedeath(double total)
{
	//calculating probability that total amount of venom kills the host
	//type 2 / 3 functional response (n >= 1); never reaching 100%
	double x = pow(total, n_death);
	return x / (x + k_death);
}
//---------------------------------------------------------------------------
void parasitize (vector<host> &h, vector<parasitoid> &p)
{
	//parasitization of hosts by parasitoids

	int chosenparasitoid;
	//keeping track of which parasitoids parasitize which hosts
	vector< vector<int> > parasitizations (h.size(), vector<int> (0,0));	
	
	//first determine for each host, each encounter which parasitoid is encountering; store these in 2D-array
	for (int i = 0; i < h.size(); i++)
	{
		for (int j = 0; j < h[i].numencounters; j++)
		{
			chosenparasitoid = RandomNumber(p.size());
			parasitizations[i].push_back(chosenparasitoid);
		}
	}
	
	//then do the encounters and determine for each: amount of venom injected, number of eggs laid
	for (int i = 0; i < h.size(); i++) //for each host
	{
		for (int j = 0; j < parasitizations[i].size(); j++) //go over each encounter one by one
		{
			if (h[i].parasitized == false) //if the host is unparasitized
			{
				p[parasitizations[i][j]].numeggs = calculateeggs(p[parasitizations[i][j]].fecundity1); //calculate number of eggs laid
				if (p[parasitizations[i][j]].numeggs > 0) //if eggs are laid
				{
					h[i].totalvenom += p[parasitizations[i][j]].venomousness1; 	//inject venom
					h[i].parasitized = true;					//the host is now parasitized
				} //if no eggs are laid, no venom is injected and the host remains unparasitized
			}
			else //if the host was already parasitized before
			{
				p[parasitizations[i][j]].numeggs = calculateeggs(p[parasitizations[i][j]].fecundity2); //calculate number of eggs laid
				h[i].totalvenom += p[parasitizations[i][j]].venomousness2; //inject venom
			}
			h[i].numeggs += p[parasitizations[i][j]].numeggs; //add the number of eggs laid in this encounter to total eggs laid in this host
		}

		if (calculatedeath(h[i].totalvenom) >= Uniform()) //determine whether total venom injected was enough to kill the host; if so
		{
			h[i].surviving = false; //host is killed
			h[i].eggsurvival = maxc / double(h[i].numeggs); //survival probability of eggs is determined by the number of eggs laid and carrying capacity
			for (int j = 0; j < parasitizations[i].size(); j++)
				p[parasitizations[i][j]].numoffspring += calculateoffspring(p[parasitizations[i][j]].numeggs, h[i].eggsurvival); //calculate number of offspring for each parasitoid
		}
	}	
	parasitizations.clear(); //remove parasitoid encounters stored
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// REPRODUCTION OF HOSTS AND PARASITOIDS; EVOLUTION OF PARASITOIDS
//---------------------------------------------------------------------------
void hostreproduction(vector<host> &h)
{
	//host reproduction
	//determine how many hosts survived parasitization
	int totalsurviving = 0;
	for (int i = 0; i < h.size(); i++)
		if (h[i].surviving == true)
			totalsurviving++;

	//calculate how many hosts there will be in the next generation: logistic growth
	int nextgen = totalsurviving * lambda * exp (-d * double(totalsurviving));
	h.resize(nextgen);
	
	//reset all hosts to initial conditions
	for (int i = 0; i < nextgen; i++)
		updatehost(h[i]);	
}
//---------------------------------------------------------------------------
void mutate(parasitoid &x)
{
	//evolution: mutation in one of the traits

	switch(RandomNumber(3)) //one of the traits is randomly chosen; then new value for that trait is drawn from normal distribution with s.d. = mutationstep
	{
		case 0: x.venomousness += Normal (0, mutationstep); break;
		case 1: x.division_f += Normal (0, mutationstep); break;
		case 2: x.division_v += Normal (0, mutationstep); break;
	}
	
	//keep venom production above 0, and both division traits between 0 and 1
	if (x.venomousness < 0)
		x.venomousness = 0;
	if (x.division_f < 0)
		x.division_f = 0;
	if (x.division_f > 1.0)
		x.division_f = 1.0;
	if (x.division_v < 0)
		x.division_v = 0;
	if (x.division_v > 1.0)
		x.division_v = 1.0;
}
//---------------------------------------------------------------------------
void parasitoidreproduction(vector<parasitoid> &p)
{
	//parasitoid reproduction: ecological dynamics and evolution
	//generate the new generation
	vector<parasitoid> newgeneration (0);
	for (int i = 0; i < p.size(); i++) //for each parasitoid in the parent generation
	{
		for (int j = 0; j < p[i].numoffspring; j++) //generate a number of new parasitoids equal to its number of offspring
		{
			newgeneration.push_back(p[i]);	//they inherit traits of the parent
			if (mutationrate > Uniform())	//with a small probability
				mutate(newgeneration.back()); //there is mutation in one of the traits
		}
	}
	p.resize(newgeneration.size());			//resize the old generation, so the new generation can be copied onto it
	calculatefirst(newgeneration.size());		//re-calculate the probability of encountering unparasitizes hosts, based on new population size
	for (int i = 0; i < newgeneration.size(); i++)	//recalculate clutch sizes and venom amounts for new generation
		updateparasitoid(newgeneration[i]);
	p = newgeneration;				//new generation becomes the new current generation
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// CALCULATE AVERAGE TRAIT VALUES FOR TIME SERIES OUTPUT
//---------------------------------------------------------------------------
double calculateaveraged(vector<parasitoid> &p)
{
	//calculate average clutch division
	double x = 0;
	for (int i = 0; i < p.size(); i++)
		x += p[i].division_f;
	x /= double(p.size());
	return x;
}
//---------------------------------------------------------------------------
double calculateaveragef1(vector<parasitoid> &p)
{
	//calculate average fecundity on unparasitized hosts
	double x = 0;
	for (int i = 0; i < p.size(); i++)
		x += p[i].fecundity1;
	x /= double(p.size());
	return x;
}
//---------------------------------------------------------------------------
double calculateaveragef2(vector<parasitoid> &p)
{
	//calculate average fecundity on parasitized hosts
	double x = 0;
	for (int i = 0; i < p.size(); i++)
		x += p[i].fecundity2;
	x /= double(p.size());
	return x;
}
//---------------------------------------------------------------------------
double calculateaveragev(vector<parasitoid> &p)
{
	//calculate average venom production
	double x = 0;
	for (int i = 0; i < p.size(); i++)
		x += p[i].venomousness;
	x /= double(p.size());
	return x;
}
//---------------------------------------------------------------------------
double calculateaveragevdiv(vector<parasitoid> &p)
{
	//calculate average venom division
	double x = 0;
	for (int i = 0; i < p.size(); i++)
		x += p[i].division_v;
	x /= double(p.size());
	return x;
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// OUTPUT: STORE RESULTS IN ARRAYS TO BE WRITTEN TO FILE LATER
//---------------------------------------------------------------------------
void plotdistribution(vector<parasitoid> &p, vector< vector<double> > &output, int generation, int run, int time)
{
	//plot the trait distributions in the population over time
	//divide trait range into 50 categories (0-0.02, 0.02-0.04, etc) and keep track of which category has highest abundance
	vector<double> traitcategories1(50);
	vector<double> traitcategories2(50);
	vector<double> traitcategories3(50);
	int highesttrait1 = 0;
	int highesttrait2 = 0;
	int highesttrait3 = 0;

	//determine for each parasitoid, for all 3 traits, in which category their trait values fall
	for (int i = 0; i < p.size(); i++)
		for (int j = 0; j < 50; j++)
			if (p[i].venomousness <= 0.02 * double(j))
			{
				traitcategories1[j]++;
				if (traitcategories1[j] > traitcategories1[highesttrait1])
					highesttrait1 = j;
				break;
			}
	for (int i = 0; i < p.size(); i++)
		for (int j = 0; j < 50; j++)
			if (p[i].division_v <= 0.02 * double(j))
			{
				traitcategories2[j]++;
				if (traitcategories2[j] > traitcategories2[highesttrait2])
					highesttrait2 = j;
				break;
			} 
	for (int i = 0; i < p.size(); i++)
		for (int j = 0; j < 50; j++)
			if (p[i].division_f <= 0.02 * double(j))
			{
				traitcategories3[j]++;
				if (traitcategories3[j] > traitcategories3[highesttrait3])
					highesttrait3 = j;
				break;
			} 
  	
	//normalize categories so that highest has an abundance of 1 (makes for better plots)
	highesttrait1 = traitcategories1[highesttrait1];
	highesttrait2 = traitcategories2[highesttrait2];
	highesttrait3 = traitcategories3[highesttrait3];
	for (int i = 0; i < 50; i++)
	{
		traitcategories1[i] /= double(highesttrait1);
		traitcategories2[i] /= double(highesttrait2);
		traitcategories3[i] /= double(highesttrait3);
	}

	//store output for this generation and this simulation run in 2D-array
	for (int i = 0; i < 50; i++)
	{
		output[0][time * 50 + i] = generation;
		output[1][time * 50 + i] = 0.02 * double(i);
		output[run * 3 + 2][time * 50 + i] = traitcategories1[i];
		output[run * 3 + 3][time * 50 + i] = traitcategories2[i];
		output[run * 3 + 4][time * 50 + i] = traitcategories3[i];
	}              
}
//--------------------------------------------------------------------------
void maketimeseries(vector< vector<double> > &output, vector<host> &h, vector<parasitoid> &p, int run, int generation)
{
	//make time series, storing host and parasitoid abundances and average trait values every 100 generations
	if (run == 0)
		output[0][generation - 1] = generation * 100;
	output[run * 5 + 1][generation - 1] = h.size();
	output[run * 5 + 2][generation - 1] = p.size();
	output[run * 5 + 3][generation - 1] = calculateaveragev(p);
	output[run * 5 + 4][generation - 1] = calculateaveragevdiv(p);
	output[run * 5 + 5][generation - 1] = calculateaveraged(p);
}
//---------------------------------------------------------------------------
void makeoutput_end(vector< vector<double> > &output, vector<host> &h, vector<parasitoid> &p, int run)
{
	//store host and parasitoid abundances and average trait values at the end of simulation
	output[run][0] = h.size();
	output[run][1] = p.size();
	output[run][2] = calculateaveragev(p);
	output[run][3] = calculateaveragevdiv(p);
	output[run][4] = calculateaveraged(p);
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

	//create output files
	string outfilename1 = argv[2];
	string outfilename2 = argv[3];
	ofstream outfile1 (outfilename1.c_str());
	ofstream outfile2(outfilename2.c_str());
	
	//set seed for random number generator
	int seed = atoi(argv[4]);
	SetSeed(seed);

	int resolution = numgenerations / 100; //resolution for plotting distribution over time

	//2D-arrays in which output is stored: time series, distribution of traits in population, averages and abundances at the end of each simulation run
	vector< vector<double> > output_timeseries(numruns * 5 + 1, vector<double> (numgenerations / 100, 0));
	vector< vector<double> > output_distribution(numruns * 3 + 2, vector<double> (5000, 0));
	vector< vector<double> > output_end(numruns, vector<double> (5, 0));

	//create hosts and parasitoids
	vector<parasitoid> parasitoids (initialparasitoids);
	vector<host> hosts (initialhosts);
	
	//the simulation, doing numruns replicates
	for (int run = 0; run < numruns; run++)
	{
		//initialize simulation
		initialize(hosts, parasitoids);

		//start simulation
		for (int i = 0; i < numgenerations; i++)
		{
			if ((i+1) % 100 == 0) //make time series every 100 generations
				maketimeseries(output_timeseries, hosts, parasitoids, run, (i+1) / 100);
			if (i % resolution == 0) //plot distribution of traits, 100 times per simulation run
				plotdistribution(parasitoids, output_distribution, i, run, i / resolution);
			distributeencounters(hosts, parasitoids); //distribute encounters over hosts
			parasitize(hosts, parasitoids);		  //parasitization
			hostreproduction(hosts);		  //host reproduction
			parasitoidreproduction(parasitoids);      //parasitoid reproduction and evolution
		}
		makeoutput_end(output_end, hosts, parasitoids, run); //store host, parasitoid abundance and average trait values at end of simulation
	}

	//--------------------------------------------
	//write results to textfiles
	//first, write end-of-simulation results to file
	for (int i = 0; i < 5; i++)
	{
		for (int j = 0; j < numruns; j++)
			outfile1 << output_end[j][i] << "\t";
	}
	outfile1 << endl;
	//use same file to store timeseries
	outfile1 << "time\t";
	for (int i = 0; i < numruns; i++)
		outfile1 << "hosts\tparasitoids\tav v\tav div v\tav div\t"; //write headers to file
	outfile1 << endl;
	for (int i = 0; i < numgenerations / 100; i++)
	{
		for (int j = 0; j < output_timeseries.size(); j++)
			outfile1 << output_timeseries[j][i] << "\t";   //write simulation results to file
		outfile1 << endl;
	}
	
	//write distributions to a separate text file
	outfile2 << "time\tvalue\t";
	for (int i = 0; i < numruns; i++)
		outfile2 << i + 1 << " v\tvdiv\tdiv\t"; //write headers to file
	outfile2 << endl;
	for (int i = 0; i < 5000; i++)
	{
		for (int j = 0; j < numruns * 3 + 2; j++)
			outfile2 << output_distribution[j][i] << "\t"; //write results to file
		outfile2 << endl;
	}
        
	return 0;
}
//---------------------------------------------------------------------------
