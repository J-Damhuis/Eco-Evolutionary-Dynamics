#ifndef EVOLUTIONDEFENSE_IND_H
#define EVOLUTIONDEFENSE_IND_H
using namespace std;

//---------------------------------------------------------------------------
// INITIALIZE SIMULATION AND CALCULATE GROWTH RATE
//---------------------------------------------------------------------------
void updateplant(plant &p)
{
	//recalculate values after mutation or after initialization of trait values
	//in this case, only needed for growth rate
	p.growthrate = basic_growthrate * (1 - (p.investment_amount * cost_growth_amount + p.investment_toxicity * cost_growth_toxicity));
}
//---------------------------------------------------------------------------
void initialize(vector<plant> &plants)
{
	//initialize trait values and other member variables for plants at the beginning of simulation
	for (int i = 0; i < numlineages; i++)
	{
		plants[i].investment_amount = initialvalue_amount;
		plants[i].investment_toxicity = initialvalue_toxicity;
		plants[i].biomass = initial_biomass / numlineages;
		plants[i].quality = basic_quality;
		plants[i].deathrate = basic_deathrate;
		updateplant(plants[i]); //calculate growth rate based on trait values
	}
}
//---------------------------------------------------------------------------
void initialize_consumption(vector<int> &c)
{
	//array of indices for plant lineages, to randomize order in which they are consumed
	//I very much doubt it makes a difference, but just in case
	for (int i = 0; i < c.size(); i++)
		c[i] = i;
}
//---------------------------------------------------------------------------
void shuffle (vector<int> &c)
{
	//randomize the order by shuffling indices
	int bla, bla2;
	for (int i = 0; i < c.size(); i++)
	{
		bla = RandomNumber(numlineages);
		bla2 = c[i];
		c[i] = c[bla];
		c[bla] = bla2;
	}
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// ECOLOGICAL DYNAMICS: PLANT GROWTH, PLANT CONSUMPTION BY HERBIVORES, HERBIVORE DEATH
//---------------------------------------------------------------------------
double sumbiomass(vector<plant> &p)
{
	//return total plant biomass
	double x = 0;
	for (int i = 0; i < numlineages; i++)
		x += p[i].biomass;
	return x;
}
//---------------------------------------------------------------------------
void plantgrowth(vector<plant> &p)
{
	//plant growth: calculate amount of growth for each lineage
	vector<double> growthrates (numlineages);
	if (competitionindex == 0) //in case of logistic growth
	{
		double c1 = sumbiomass(p);
		for (int i = 0; i < numlineages; i++)
			growthrates[i] = updaterate * (p[i].growthrate * (1 - c1 / K)) * p[i].biomass;
	}
	else //in case of nutrient competition
		for (int i = 0; i < numlineages; i++)
			growthrates[i] = updaterate * (p[i].growthrate * free_nutrients / (free_nutrients + growth_constant) - p[i].deathrate) * p[i].biomass;

	//then add to biomass for that lineage
	for (int i = 0; i < numlineages; i++)
	{
		p[i].biomass += growthrates[i];
		if (competitionindex == 1) //in case of nutrient competition, take up nutrients for growth from free nutrient pool
			free_nutrients -= growthrates[i] * p[i].quality * conversionfactor;
	}
}
//---------------------------------------------------------------------------
double calculatehandlingtime(vector<plant> &p)
{
	//calculate and return total handling time of all plant lineages
	double x = 0;
	for (int i = 0; i < numlineages; i++)
		x += attackrate * p[i].biomass;
	x *= basic_handlingtime;
	return x;
}
//---------------------------------------------------------------------------
double calculateamount (double animalbiomass, double plantbiomass, double totalhandlingtime, double defense)
{
	//calculate and return amount consumed of a lineage, depending on herbivore and plant biomass, and level of defense
	return (1.0 / (1.0 + defense * effect_amount)) * animalbiomass * plantbiomass * attackrate / (1.0 + totalhandlingtime);
}
//---------------------------------------------------------------------------
void consumption(vector<plant> &p, vector<int> c)
{
	//herbivore death and consumption of plants
	//first, calculate total handling time
	double totalhandlingtime = calculatehandlingtime(p);

	//in case of specialist herbivores
	if (herbivoredynamicsindex == 1)
	{
		//herbivore mortality
		if (competitionindex == 1) //in case of nutrient competition, return nutrients to free nutrient pool
			free_nutrients += updaterate * conversionfactor * deathrate_animals * herbivorebiomass;
		herbivorebiomass -= updaterate * deathrate_animals * herbivorebiomass;
		
		double totaltoxicity = 0;		//keep track of extra mortality due to consuming toxic plant material
		double biomass = herbivorebiomass;	//used to calculate all amounts consumed; otherwise, plants consumed last would be consumed more
		double amounteaten;			//amount consumed of a given lineage
		
		//consumption of all lineages
		for (int i = 0; i < numlineages; i++)
		{
			amounteaten = updaterate * calculateamount(biomass, p[c[i]].biomass, totalhandlingtime, p[c[i]].investment_amount);
			totaltoxicity += amounteaten * p[c[i]].investment_toxicity * effect_toxicity;
			p[c[i]].biomass -= amounteaten;
			herbivorebiomass += amounteaten * p[c[i]].quality * conversion_efficiency;
			if (competitionindex == 1)
				free_nutrients += amounteaten * p[c[i]].quality * conversionfactor * (1.0 - conversion_efficiency);
		}
		//if toxic plant biomass has been consumed, extra mortality
		if (competitionindex == 1)
			free_nutrients += totaltoxicity * conversionfactor;
		herbivorebiomass -= totaltoxicity;
	}
	//in case of generalist herbivores, herbivore biomass doesn't change unless there is toxicity (which lowers herbivore biomass in the next time step)
	else
	{
		double totaltoxicity = 0;	//keep track of mortality due to consuming toxic plant material
		double amounteaten;		//amount consumed of a given lineage

		//consumption of all lineages
		//note that here ALL consumed plant material goes back to the nutrient pool, because herbivore biomass is left out of the total nutrient pool
		for (int i = 0; i < numlineages; i++)
		{
			amounteaten = updaterate * calculateamount(herbivorebiomass, p[c[i]].biomass, totalhandlingtime, p[c[i]].investment_amount);
			p[c[i]].biomass -= amounteaten;
			totaltoxicity += amounteaten * p[c[i]].investment_toxicity * effect_toxicity;
			if (competitionindex == 1)
				free_nutrients += amounteaten * p[c[i]].quality * conversionfactor;
		}
		//mortality due to consuming toxic plant material: reduces herbivore biomass in the next time step, but the calculation starts from basic_herbivorebiomass each time
		herbivorebiomass = basic_herbivorebiomass - totaltoxicity;
		if (herbivorebiomass < 0)
			herbivorebiomass = 0;
	}
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// EVOLUTIONARY DYNAMICS: MUTATION
//---------------------------------------------------------------------------
void mutatetrait(double &x, double step, double min, double max)
{
	//drawing new trait value from normal distribution; cut off at 0 and a set maximum
	x += Normal(0, step);
	if (x < min)
		x = min;
	if (x > max)
		x = max;
}
//---------------------------------------------------------------------------
int findlowest (vector<plant> &p, int x)
{
	//find and return plant lineage that has lowest biomass (has to be other than x, which is the parent lineage)
	int a;
	if (x == 0)
		a = 1;
	else a = 0;
	for (int i = 0; i < numlineages; i++)
		if (p[i].biomass < p[a].biomass && i != x)
		a = i;
	return a;
}
//---------------------------------------------------------------------------
void mutate(vector<plant> &p, int x)
{
	//mutate lineage x: create a daughter lineage which gets all traits of x. Equally divide biomass of x over the two.
	plant a = p[x];
	p[x].biomass = p[x].biomass / 2.0;
	a.biomass = a.biomass / 2.0;
        
	//draw number from uniform distribution to determine which trait will mutate
	double trait = Uniform();
	if (trait <= mutationrate_amount / totalmutationrate)
		mutatetrait(a.investment_amount, mutationstep, 0.00001, maxvalue_amount);
	else mutatetrait(a.investment_toxicity, mutationstep, 0.00001, maxvalue_toxicity);

	updateplant(a); //recalculate growth rate after mutation

	//find lineage with lowest biomass; it is removed from the population and the new daughter lineage takes its place
	int b = findlowest(p, x);
	if (competitionindex == 1)
		free_nutrients += p[b].biomass * p[b].quality * conversionfactor; 
	p[b] = a;
}
//---------------------------------------------------------------------------
void mutation(vector<plant> &p)
{
	//mutation: go over all lineages, mutation occurs with probability "totalmutationrate"
	for (int i = 0; i < numlineages; i++)
		if (totalmutationrate * updaterate > Uniform() && p[i].biomass > 100)
			mutate(p, i);
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// STORING SIMULATION RESULTS INTO ARRAYS DURING SIMULATION RUNS, TO WRITE TO FILE LATER AFTER ALL RUNS ARE DONE
//---------------------------------------------------------------------------
void generateoutput(vector<plant> &p, vector< vector<double> > &output, int run, int generation, int time)
{
	//store trait distribution over time in array to save to file later
	//this particular output format is was chosen because it is well suited for making contour graphs in Sigmaplot
	//distribution values for each trait for simulation run are stored in one column of the array

	//divide trait range into 50 categories; each lineage will fall into one of them. Also keep track of most abundant category to normalize them later
	vector<double> traitcategories1(50);
	vector<double> traitcategories2(50);
	int highesttrait1 = 0;
	int highesttrait2 = 0;

	//go over each plant lineage; determine into which category the trait value falls
	for (int i = 0; i < numlineages; i++)
		for (int j = 0; j < 50; j++)
			if (p[i].investment_amount <= 0.02 * double(j+1) * maxvalue_amount)
			{
				traitcategories1[j]++;
				if (traitcategories1[j] > traitcategories1[highesttrait1])
					highesttrait1 = j;
				break;
			}        
	for (int i = 0; i < numlineages; i++)
		for (int j = 0; j < 50; j++)
			if (p[i].investment_toxicity <= 0.02 * double(j+1) * maxvalue_toxicity)
			{
				traitcategories2[j]++;
				if (traitcategories2[j] > traitcategories2[highesttrait2])
					highesttrait2 = j;
				break;
			}
	
	//normalize the category values so that the most abundant trait category has a value of 1 (makes for better graphs)
	highesttrait1 = traitcategories1[highesttrait1];
	highesttrait2 = traitcategories2[highesttrait2];
	for (int i = 0; i < 50; i++)
	{
		traitcategories1[i] /= double(highesttrait1);
		traitcategories2[i] /= double(highesttrait2);
	}

	//store results in array
	for (int i = 0; i < 50; i++)
	{
		output[0][(generation - 1) * 50 + i] = generation * time;
		output[1][(generation - 1) * 50 + i] = 0.02 * double(i+1);
		output[2][(generation - 1) * 50 + i] = 0.02 * double(i+1) * maxvalue_amount;
		output[2*run+3][(generation - 1) * 50 + i] = traitcategories1[i];
		output[2*run+4][(generation - 1) * 50 + i] = traitcategories2[i];
	}
}
//---------------------------------------------------------------------------
void generateindvalues(vector<plant> &p, vector< vector<double> > &output, int run)
{
	//at the end of each simulation run, store individual values for each lineage for defense and biomass
	//the other two defense traits are disregarded because they never evolve to a nonzero value
	for (int i = 0; i < numlineages; i++)
	{
		output[run * 2][i] = p[i].biomass;
		output[run * 2 + 1][i] = p[i].investment_amount;
	}
}
//---------------------------------------------------------------------------
// WRITING RESULTS TO FILES
//---------------------------------------------------------------------------
void writeoutput (vector< vector<double> > &output, vector <vector<double> > &output2, ofstream &outfile, ofstream &outfile2)
{
	//write trait distributions to file
	//first write headers
	outfile << "generation\tvalue1\tvalue2\t";
	for (int i = 0; i < numrounds; i++)
		outfile << "amount\ttoxic\t";
	outfile << endl;
	//then write results
	for (int i = 0; i < 5000; i++)
	{
		for (int j = 0; j < output.size(); j++)
			outfile << output[j][i] << "\t";
		outfile << endl;
	}
	
	//write headers for individual values (for amount only) and biomass 
	for (int i = 0; i < numrounds; i++)
		outfile2 << "biomass\tamount\t";
	outfile2 << endl;
	//write results
	for (int i = 0; i < numlineages; i++)
	{
		for (int j = 0; j < numrounds * 2; j++)
			outfile2 << output2[j][i] << "\t";
		outfile2 << endl;
	}
}   
//---------------------------------------------------------------------------

#endif
 