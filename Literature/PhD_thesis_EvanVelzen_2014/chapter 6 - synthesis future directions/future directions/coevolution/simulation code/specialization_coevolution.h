#ifndef SPECIALIZATION_H
#define SPECIALIZATION_H
#include "specialization_parameters_coevolution.h"
using namespace std;

//---------------------------------------------------------------------------
// CALCULATING VARIOUS SMALL THINGS
//---------------------------------------------------------------------------
int minimum (int a, int b)
{
        if (a < b)
                return a;
        return b;
}
//---------------------------------------------------------------------------
void calculatespecialization(animal &herbivore)
{
	//calculates how specialized a given lineage is; 0 = equal effort to each plant species; 1 = completely specialized on one plant species
	double sp = 0;
	for (int i = 0; i < number_plantspecies; i++)
		sp += pow(herbivore.preference[i] / herbivore.totalpreference - 1.0 / double(number_plantspecies), 2.0) * number_plantspecies / (number_plantspecies - 1);

	herbivore.specialization = sp;
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// INITIALIZATION OF HERBIVORES AND PLANTS
//---------------------------------------------------------------------------
void initializeanimals(vector<animal> &herbivores)
{
	//give herbivores initial values for preference (drawn from normal distribution with mean 0.5 (complete generalist)) and initial biomass
	for (int i = 0; i < number_herbivores; i++)
	{
		for (int j = 0; j < number_plantspecies; j++)
		{
			herbivores[i].preference[j] = Normal(0.5, deviation);
			if (herbivores[i].preference[j] < 0)
				herbivores[i].preference[j] = 0;
			if (herbivores[i].preference[j] > 1.0)
				herbivores[i].preference[j] = 1.0;
			herbivores[i].totalpreference += herbivores[i].preference[j];
		}
		calculatespecialization(herbivores[i]);
		herbivores[i].biomass = initial_herbivorebiomass / double(number_herbivores);
		herbivores[i].totaltoxicity = 0;
	}
}
//---------------------------------------------------------------------------
void initializeconsumers(vector<int> &consumers)
{
	//set up array of indices of herbivores, for randomization later 
	for (int i = 0; i < consumers.size(); i++)
                consumers[i] = i;
}
//---------------------------------------------------------------------------
void shuffle (vector<int> &stuff)
{
	//shuffle array of indices to randomize order in which herbivores consume plants 
	int bla2;
        for (int i = 0; i < stuff.size(); i++)
        {
                int bla = RandomNumber(stuff.size());
                bla2 = stuff[i];
                stuff[i] = stuff[bla];
                stuff[bla] = bla2;
        }
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// ECOLOGICAL DYNAMICS: PLANT GROWTH, HERBIVORE CONSUMPTION AND DEATH
//---------------------------------------------------------------------------
double plantgrowth(double growthrate, double plantbiomass, double &free_nutrients)
{
	//normal plant growth, neutral competition
	double amountofgrowth = updaterate * (growthrate * free_nutrients / (free_nutrients + total_nutrients) - deathrate_plants);
	return amountofgrowth * plantbiomass;
}
//---------------------------------------------------------------------------
double plantgrowth_ratio(double growthrate, double plantbiomass, double &free_nutrients, double competition)
{
	//ratio dependent plant growth
	double amountofgrowth = updaterate * (growthrate * free_nutrients / (growth_constant * free_nutrients + competition) - deathrate_plants);
	return amountofgrowth * plantbiomass;
}
//---------------------------------------------------------------------------
double calculatecompetition(vector<plant> &plants, int x) 
{
	//calculate how much competition is suffered from the other plants, in case of ratio dependent growth
	double comp = 0;
	for (int i = 0; i < plants.size(); i++)
	{
		if (i == x)
			comp += a1 * plants[i].biomass; //intraspecific competition
		else comp += a2 * plants[i].biomass;	//interspecific competition
	}
	return comp;
}
//---------------------------------------------------------------------------
double eating(double &plantbiomass, double attractiveness, double toxicity, int plant, animal &herbivore, double totalhandlingtime, double &free_nutrients)
{
	//calculate amount consumed
        double amounteaten = updaterate * herbivore.biomass * attackrate * attractiveness * pow(herbivore.preference[plant] / herbivore.totalpreference, n)
                * plantbiomass / (1.0 + totalhandlingtime);

	//if that amount is less than total plant biomass, carry on as usual
        if (amounteaten <= plantbiomass)
        {
		herbivore.biomass += amounteaten * quality * conversion_efficiency;
		herbivore.totaltoxicity += amounteaten * toxicity;
		free_nutrients += amounteaten * conversionfactor * (1.0 - conversion_efficiency) * quality;
		return amounteaten;
        }

	//if that amount is more than total plant biomass: amount consumed is equal to total plant biomass
        amounteaten = plantbiomass;
        herbivore.biomass += amounteaten * quality * conversion_efficiency;
	herbivore.totaltoxicity += amounteaten * toxicity;
        free_nutrients += amounteaten * conversionfactor * (1.0 - conversion_efficiency) * quality;;
        return amounteaten;
}
//---------------------------------------------------------------------------
void ecologicaldynamics(vector<plant> &plants, vector<animal> &herbivores, vector<int> &cons, double &free_nutrients)
{
	//all ecological dynamics: plant growth and death, plant consumption by herbivores and herbivore death

	//plant growth: first calculate all growthrates; assuming simultaneous growth they should have access to the same nutrient pool. Then do the actual growth.
	vector<double> growthrates(number_plantspecies);    
	for (int i = 0; i < plants.size(); i++)
	{
		if (version == 0) //in case of "normal" not-competition growth
			growthrates[i] = plantgrowth(plants[i].growthrate, plants[i].biomass, free_nutrients);
		else growthrates[i] = plantgrowth_ratio(plants[i].growthrate, plants[i].biomass, free_nutrients, calculatecompetition(plants, i)); //in case of ratio-dependent growth
	}
	for (int i = 0; i < plants.size(); i++)
	{
		plants[i].biomass += growthrates[i];
		free_nutrients -= quality * conversionfactor * growthrates[i];
	}

	//plant consumption by herbivores
        for (int i = 0; i < herbivores.size(); i++)
        {
                double totalhandlingtime = 0;
                for (int j = 0; j < plants.size(); j++)
                        totalhandlingtime += handlingtime * attackrate
                        * pow(herbivores[cons[i]].preference[j] / herbivores[cons[i]].totalpreference, n) * plants[j].biomass;
                for (int j = 0; j < plants.size(); j++)
                        plants[j].biomass -= eating(plants[j].biomass, plants[j].attractiveness, plants[j].toxicity, j, herbivores[cons[i]], totalhandlingtime, free_nutrients);

        }

	//herbivore death
        for (int i = 0; i < herbivores.size(); i++)
	{
		free_nutrients += updaterate * deathrate_animals * conversionfactor * herbivores[i].biomass;
		herbivores[i].biomass -= updaterate * deathrate_animals * herbivores[i].biomass;
		free_nutrients += herbivores[i].totaltoxicity * conversionfactor;
		herbivores[i].biomass -= herbivores[i].totaltoxicity;
		herbivores[i].totaltoxicity = 0;
        }
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// EVOLUTIONARY DYNAMICS: MUTATION
//---------------------------------------------------------------------------
// PLANTS
//---------------------------------------------------------------------------
double calculatetotalattack(vector<animal> &herbivores, plant &p1, plant &p2, int identity)
{
	double attack1, attack2, totalhandlingtime;
	double total = 0;
	for (int i = 0; i < number_herbivores; i++)
	{
		attack1 = attackrate * pow(herbivores[i].preference[0] / herbivores[i].totalpreference, n) * p1.biomass;
		attack2 = attackrate * pow(herbivores[i].preference[1] / herbivores[i].totalpreference, n) * p2.biomass;
		totalhandlingtime = (attack1 + attack2) * handlingtime;	
		if (identity == 0)
			total += herbivores[i].biomass * p1.attractiveness * attack1 / (1 + totalhandlingtime);
		else total += herbivores[i].biomass * p2.attractiveness * attack2 / (1 + totalhandlingtime);
	}
	return total;
}
//---------------------------------------------------------------------------
void updateplant(plant &p)
{
	//re-calculate growth rate, defense against consumption and toxicity after mutation
	p.attractiveness = 1.0 / (1.0 + p.defense * effect_amount);
	p.growthrate = basic_growthrate * (1.0 - costliness * p.defense);
	p.toxicity = p.defense * effect_toxicity;
}
//---------------------------------------------------------------------------
void mutatep(plant &p, int identity)
{
	//introduce mutation
	//new value is drawn from a normal distribution around the old one; the two plants have separate mutation step sizes (determining speed of evolution)
	if (identity == 0)
		p.defense += Normal(0, mutationstep_plants_1);
	else p.defense += Normal(0, mutationstep_plants_2);
	if (p.defense < 0)
		p.defense = 0;
	updateplant(p);
}
//---------------------------------------------------------------------------
void mutateplant(vector<plant> &p, vector<animal> &herbivores, int identity, double &free_nutrients)
{
	//calculate fitness of a new mutant with a defense trait value slightly higher or slightly lower than the resident value
	//identity = plant 1 (0) or plant 2 (1)
	double totaleaten = calculatetotalattack(herbivores, p[0], p[1], identity);

	//if plant 1 is evolving
	if (identity == 0)
	{
		//calculate fitness of resident (net growth rate; growth - death - consumption by herbivores)
		p[0].fitness = p[0].growthrate * free_nutrients / (free_nutrients + total_nutrients) - deathrate_plants - totaleaten / p[0].biomass;
		
		//generate mutant with a slightly different value for defense
		plant pnew = p[0];
		mutatep(pnew, identity);

		//calculate fitness of mutant
		totaleaten = calculatetotalattack(herbivores, pnew, p[1], identity);

		//if fitness is higher than that of resident, resident defense value is replaced with that of mutant
		pnew.fitness = pnew.growthrate * free_nutrients / (free_nutrients + total_nutrients) - deathrate_plants - totaleaten / pnew.biomass;
		if (pnew.fitness > p[0].fitness)
			p[0] = pnew;
	}
	//if plant 2 is evolving: follow same steps as outlined above
	if (identity == 1)
	{
		p[1].fitness = p[1].growthrate * free_nutrients / (free_nutrients + total_nutrients) - deathrate_plants - totaleaten / p[1].biomass;
		plant pnew = p[1];
		mutatep(pnew, identity);
		totaleaten = calculatetotalattack(herbivores, p[0], pnew, identity);
		pnew.fitness = pnew.growthrate * free_nutrients / (free_nutrients + total_nutrients) - deathrate_plants - totaleaten / pnew.biomass;
		if (pnew.fitness > p[1].fitness)
			p[1] = pnew;
	}
}
//---------------------------------------------------------------------------
// HERBIVORES
//---------------------------------------------------------------------------
void mutate(animal &herbnew)
{
	//introduce mutation in daughter lineage
	for (int i = 0; i < number_plantspecies; i++)	//some mutation is introduced for all preference traits
	{
		herbnew.preference[i] += Normal(0, mutationstep);
		if (herbnew.preference[i] < 0.0001)
			herbnew.preference[i] = 0.0001;
		if (herbnew.preference[i] > 1.0)
			herbnew.preference[i] = 1.0;
	}
	herbnew.totalpreference = 0;
	for (int i = 0; i < number_plantspecies; i++)
		herbnew.totalpreference += herbnew.preference[i]; //normalize all preferences so they add up to 1 again
	calculatespecialization(herbnew); 			  //calculate how specialized this lineage is
}
//---------------------------------------------------------------------------
void removelast(vector<animal> &herbivores, double &free_nutrients)
{
	//find herbivore lineage with lowest biomass and remove from population

	//go through population, find lineage with lowest biomass 
	double lowestbiomass = 1000000;
	int lowest = 0;
	for (int i = 0; i < number_herbivores; i++)
	{
		if (herbivores[i].biomass < lowestbiomass)
		{
			lowestbiomass = herbivores[i].biomass;
			lowest = i;
		}
	}
	free_nutrients += herbivores[lowest].biomass * conversionfactor; 	//add nutrients bound up in this lineage to free nutrient pool
	herbivores.erase(herbivores.begin()+lowest);				//remove lineage from population
}
//---------------------------------------------------------------------------
void mutation(vector<animal> &herbivores, double &free_nutrients, double p)
{
	//main mutation function; go through population; mutation occurs in each lineage with a small probability
	for (int i = 0; i < herbivores.size(); i++)
                if (mutationrate * p > Uniform() && herbivores[i].biomass > 500) 	//assuming lineages with very small biomass can't split into two viable daughter lineages
                {
                        herbivores.push_back(herbivores[i]);				//add new lineage to population
                        herbivores.back() = herbivores[i];				//which gets all trait and other values from parent lineage
                        herbivores.back().biomass = herbivores.back().biomass / 2.0; 	//divide daughter lineage by half (gets only half the biomass)
                        herbivores[i].biomass = herbivores[i].biomass / 2.0;		//divide parent lineage biomass by half as well
                        mutate(herbivores.back());					//slight mutation in new lineage's preference traits
                        removelast(herbivores, free_nutrients);				//find lineage with lowest biomass and remove, so total number of lineages remains constant
                }
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// STORING SIMULATION RESULTS INTO ARRAYS DURING SIMULATION RUNS, TO WRITE TO FILE LATER AFTER ALL RUNS ARE DONE
//---------------------------------------------------------------------------
//used in type A, abundance series
void generateabundanceseries(vector< vector<double> > &output, vector<plant> &plants, vector<animal> &herbivores, double &free_nutrients, int time, int replica)
{
	//add up biomass of all herbivores
	double totalherbivorebiomass = 0;
	for (int i = 0; i < number_herbivores; i++)
		totalherbivorebiomass += herbivores[i].biomass;

	//store abundances of plants and herbivores, total free nutrients and plant defense levels to write to file later 
	if (replica == 0)
		output[0][time - 1] = time * 10;
	output[(number_plantspecies * 2 + 2) * replica + number_plantspecies * 2 + 1][time - 1] = totalherbivorebiomass;
	output[(number_plantspecies * 2 + 2) * replica + number_plantspecies * 2 + 2][time - 1] = free_nutrients;
	for (int i = 0; i < number_plantspecies; i++)
	{
		output[(number_plantspecies * 2 + 2) * replica + i + 1][time - 1] = plants[i].biomass;
		output[(number_plantspecies * 2 + 2) * replica + number_plantspecies + i + 1][time - 1] = plants[i].defense;

	}
}
//---------------------------------------------------------------------------
void analyzepreferences(vector<animal> &herbivores) //analyzes individual values of all herbivores, divides them into species based on how similar they are.
{
        vector< vector<int> > discretevalues(number_plantspecies, vector<int> (number_herbivores, 0));
        vector<int> indexes(number_herbivores);
        vector<double> preferencevalues(number_herbivores);
        vector<unsigned long int> totalvalues(number_herbivores);
        vector<unsigned long int> totalvalues2(number_herbivores);

        int lowesti; double lowest;
        for (int j = 0; j < number_plantspecies; j++)
        {
                for(int i = 0; i < number_herbivores; i++)
                        preferencevalues[i] = herbivores[i].preference[j] / herbivores[i].totalpreference;
                for (int i = 0; i < number_herbivores; i++)
                {
                        lowest = 2;
                        for (int k = 0; k < number_herbivores; k++)
                                if (preferencevalues[k] <= lowest && preferencevalues[k] != -1)
                                {
                                        lowest = preferencevalues[k];
                                        lowesti = k;
                                }
                        indexes[i] = lowesti;
                        preferencevalues[lowesti] = -1;
                }
                double threshold = 0.2;
                double value = herbivores[indexes[0]].preference[j]/herbivores[indexes[0]].totalpreference;
                int index = 1;
                for (int i = 0; i < number_herbivores; i++)
                {
                        if (herbivores[indexes[i]].preference[j] / herbivores[indexes[i]].totalpreference - value > threshold
                                || value - herbivores[indexes[i]].preference[j] / herbivores[indexes[i]].totalpreference > threshold)
                        {
                                index++;
                                value = herbivores[indexes[i]].preference[j] / herbivores[indexes[i]].totalpreference;
                        }
                        discretevalues[j][indexes[i]] = index;
                }
        }

        for (int j = 0; j < number_plantspecies; j++)
                for (int i = 0; i < number_herbivores; i++)
                {
                        totalvalues[i] += pow(10, j) * discretevalues[j][i];
                        totalvalues2[i] += pow(10, j) * discretevalues[j][i];
                }
        unsigned long int highestsp;
        int highest = -1;
        for (int i = 0; i < number_herbivores; i++)
        {
                highestsp = 0;
                for (int k = 0; k < number_herbivores; k++)
                        if (totalvalues2[k] >= highestsp && totalvalues2[k] != -1)
                        {
                                highestsp = totalvalues2[k];
                                highest = k;
                        }
                indexes[number_herbivores - 1 - i] = highest;
                totalvalues2[highest] = -1;
        }

        int index = 1;
        unsigned long int spvalue = totalvalues[indexes[0]];
        for (int i = 0; i < number_herbivores; i++)
        {
                if (totalvalues[indexes[i]] > spvalue)
                {
                        index++;
                        spvalue = totalvalues[indexes[i]];
                }
                herbivores[indexes[i]].speciesnumber = index;
        }
}
//---------------------------------------------------------------------------
//used in file type A, individual values
void generateindividualvalues(vector< vector<double> >&output, vector<animal> &herbivores, int replica)
{
        analyzepreferences(herbivores);	 //analyze which herbivores belong to the same species
        
	//ordering the herbivores by species, to make the output file more readable
	vector<int> index(number_herbivores);
        vector<int> speciesnumbers(number_herbivores);
        for(int i = 0; i < number_herbivores; i++)
                speciesnumbers[i] = herbivores[i].speciesnumber;
        int highesti; int highest;
        for (int i = 0; i < number_herbivores; i++)
        {
                highest = 0;
                for (int j = 0; j < number_herbivores; j++)
                        if (speciesnumbers[j] >= highest)
                        {
                                highest = speciesnumbers[j];
                                highesti = j;
                        }
                index[number_herbivores - 1 - i] = highesti;
                speciesnumbers[highesti] = -1;
        }

	//save individual values for herbivore species, preference, specialization, and biomass to write to file later
        for (int i = 0; i < number_herbivores; i++)
        {
                output[i][(number_plantspecies + 3) * replica] = herbivores[index[i]].speciesnumber;
                for (int j = 0; j < number_plantspecies; j++)
                        output[i][(number_plantspecies + 3) * replica + j + 1] = herbivores[index[i]].preference[j] / herbivores[index[i]].totalpreference;
                output[i][(number_plantspecies + 3) * replica + number_plantspecies + 1] = herbivores[index[i]].specialization;
                output[i][(number_plantspecies + 3) * replica + number_plantspecies + 2] = herbivores[index[i]].biomass;
        }
}
//---------------------------------------------------------------------------
void output_timeseriessp (vector<animal> &herbivores, vector< vector<double> > &output, int generation, int outputrate, int replica)
{
	//store trait distribution over time in array to save to file later
	//this particular output format is was chosen because it is well suited for making contour graphs in Sigmaplot
	//distribution values for each simulation run are stored in one column of the array, with the corresponding values for time and trait value stored in the first two columns

	//divide trait range into 50 categories; each lineage will fall into one of them. Also keep track of most abundant category to normalize them later
	vector< vector<double> > values_preference (number_plantspecies, vector<double> (numoutputcategories, 0));
	vector<double> values_specialization(numoutputcategories);
	vector<int> highestpreference(number_plantspecies);
	int highestspecialization = 0;
	for (int i = 0; i < number_plantspecies; i++)
		highestpreference[i] = 0;

	for (int i = 0; i < herbivores.size(); i++)
	{
		for (int k = 0; k < number_plantspecies; k++) //for each plant species, determine where the preference trait falls
		{
			for (int j = 0; j < numoutputcategories; j++)
				if ((herbivores[i].preference[k] / herbivores[i].totalpreference) <= double (j+1) / double(numoutputcategories))
				{
					values_preference[k][j]++; // add 1 to category if preference falls within its range
					//then keep track of which category has highest number of lineages in it
 					if (values_preference[k][j] >= values_preference[k][highestpreference[k]])
						highestpreference[k] = j;
					break;
				}
		}
		for (int j = 0; j < numoutputcategories; j++) //same for specialization index
			if (herbivores[i].specialization <= double (j+1) / double (numoutputcategories))
			{
				values_specialization[j]++;
				if (values_specialization[j] >= values_specialization[highestspecialization])
					highestspecialization = j;
				break;
			}
	}
	
	//normalize the category values so that the most abundant trait category has a value of 1 (makes for better graphs)
	for (int i = 0; i < number_plantspecies; i++)
	{
		highestpreference[i] = values_preference[i][highestpreference[i]];
		for (int j = 0; j < numoutputcategories; j++)
			values_preference[i][j] /= double(highestpreference[i]);
	}
	highestspecialization = values_specialization[highestspecialization];
	for (int i = 0; i < numoutputcategories; i++)
		values_specialization[i] /= double(highestspecialization);

	//then store the distribution to write to file later
	for (int i = 0; i < numoutputcategories; i++)
	{
		if (replica == 0) //store time and the values for the trait distribution in first two columns of the array
		{
			output[0][(generation - 1) * numoutputcategories + i] = generation * outputrate;
			output[1][(generation - 1) * numoutputcategories + i] = double (i + 1) / double (numoutputcategories);
		}
		for (int j = 0; j < number_plantspecies; j++)
			output[(number_plantspecies + 1) * replica + 2 + j][(generation - 1) * numoutputcategories + i] = values_preference[j][i];
		output[(number_plantspecies + 1) * replica + 2 + number_plantspecies][(generation - 1) * numoutputcategories + i] = values_specialization[i];
	}
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// WRITING SIMULATION RESULTS TO FILES
//---------------------------------------------------------------------------
void output_sp (ofstream &outfile1, vector< vector<double> > &output) //write trait distribution timeseries to textfile
{
	//write headers to file
	outfile1 << "time\tvalue\t";
	for (int j = 0; j < number_replicas; j++)
	{
		for (int i = 0; i < number_plantspecies; i++)
			outfile1 << "plant " << i + 1 << "\t";
		outfile1 << "specialization\t";
	}
	outfile1 << endl;
	
	//write results to file
	for (int i = 0; i < numoutputcategories * 100; i++)
	{
		for (int j = 0; j < (number_plantspecies + 1) * number_replicas + 2; j++)
			outfile1 << output[j][i] << "\t";
		outfile1 << endl;
	}
}
//---------------------------------------------------------------------------
void output_indvalues(ofstream &outfile1, vector< vector<double> > &output) //write individual values and biomass at the end of simulation to textfile
{
	//write headers to file
	outfile1 << "herbivore\t";
	for (int j = 0; j < number_replicas; j++)
	{
		outfile1 << "species\t";
		for (int i = 0; i < number_plantspecies; i++)
			outfile1 << "preference " << i + 1 << "\t";
		outfile1 << "specialization\tbiomass\t";
	}
	outfile1 << endl;
	
	//write results to file
	for (int i = 0; i < number_herbivores; i++)
	{
		outfile1 << i + 1 << "\t";
		for (int j = 0; j < (number_plantspecies + 3) * number_replicas; j++)
			outfile1 << output[i][j] << "\t";
		outfile1 << endl;
	}
}
//---------------------------------------------------------------------------
void output_abundanceseries (ofstream &outfile1, vector< vector<double> > &output)  //write abundance / plant defense timeseries to textfile
{
	//write headers to file
	outfile1 << "time\t";
	for (int n = 0; n < number_replicas; n++)
	{
		for (int i = 0; i < number_plantspecies; i++)
		{
			outfile1 << "plant " << i + 1 << "\t";
			outfile1 << "defense " << i + 1 << "\t";
		}
		outfile1 << "herbivore biomass\tfree nutrients\t";
	}
	outfile1 << endl;
	
	//write results to file
	for (int i = 0; i < number_timesteps / 10; i++)
	{
		for (int j = 0; j < (number_plantspecies * 2 + 2) * number_replicas + 1; j++)
			outfile1 << output[j][i] << "\t";
		outfile1 << endl;
	}
}
//---------------------------------------------------------------------------



#endif
 