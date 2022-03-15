#include <iostream>
#include <vector>
#include <random>
#include <fstream>
#include <algorithm>
#include <iomanip>

const int n = 1000; //Number of individuals
const int g = 50000; //Number of generations
const int h = 2; //Number of habitats
const int r = 2; //Number of resources
const int d = 10; //Number of feeding rounds per generation
const double mu = 0.001; //Mutation rate
const double sigma = 0.01; //Standard deviation of Gaussian distribution the size of a mutation is taken from
const double m = 0.1; //Migration rate
double beta = 0.5; //Degree of optimal choice
double s = 1; //Selection coefficient
double delta = 0.2; //Slope of Michaelis-Menten function for resource acquisition
double q = 0.0; //Habitat asymmetry
int seed = 1;
const int save = 1; //Save feeding efficiencies to file every so many generations
const std::vector<double> R = {10.0, 10.0}; //Total size of resources
std::ofstream ofs("data.csv"); //Output file with habitat dynamics over time
std::ofstream ofs2("heatmap.csv"); //Output file with individuals' trait values over time
std::ofstream ofs3("habitat.csv"); //Output file with individuals' habitats over time
std::mt19937_64 rng;

class Individual {
public:
    double FeedEff; //Ecological trait value
    double Food; //Found food (fitness)
    int Habitat; //Habitat individual is currently in
};

class Resource {
public:
    double Size; //Total size of the resource within a habitat
    double Sum; //Sum of trait values of individuals feeding on this resource (only used to keep track of mean trait value within resource)
    double Indiv; //Total number of individuals feeding on this resource (only used to keep track of mean number of individuals on each resource)
    double FoundFood; //Total amount of this resource found during a feeding season
};

class Habitat {
public:
    std::vector<Resource> Resources; //Resources within a habitat
    std::vector<double> SumFeedEff; //Sum of feeding efficiencies of individuals feeding on resources within a habitat (used for decision making)
    std::vector<std::vector<int> > Ind; //Lists of individuals feeding on resources within a habitat
};

std::vector<Individual> createPopulation() {
    std::vector<Individual> Population(n);
    std::uniform_real_distribution<double> chooseValue(-1.0, -0.8);
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        Population[individual].FeedEff = chooseValue(rng); //Set trait value to random number between -1.0 and -0.8
        Population[individual].Food = 0.0; //Set found food to 0
        Population[individual].Habitat = 0; //Place individual in first habitat
    }
    return Population;
}

std::vector<Habitat> createHabitats() {
    std::vector<Habitat> Habitats(h);
    for (int habitat = 0; habitat < static_cast<int>(Habitats.size()); ++habitat) {
        std::vector<Resource> Resources(r); //Create resources for habitats
        for (int resource = 0; resource < static_cast<int>(Resources.size()); ++resource) {
            Resources[resource].Size = habitat == resource ? R[resource] * (1.0 / h) * (1.0 + q) : R[resource] * (1.0 / h) * (1.0 - q); //Distribute resources over habitats according to habitat asymmetry q
            Resources[resource].Sum = 0.0; //Set sum of trait values of each resource to 0
            Resources[resource].FoundFood = 0.0; //Set total found food of each resource to 0
            Resources[resource].Indiv = 0; //Set number of individuals feeding on each resource to 0
        }
        Habitats[habitat].Resources = Resources; //Put resources in habitats
        Habitats[habitat].SumFeedEff = {0.0, 0.0}; //Set feeding efficiencies to 0
        Habitats[habitat].Ind = {{}, {}}; //Make lists of individuals feeding on the resources empty
    }
    return Habitats;
}

void shuffle(std::vector<Individual> &Population) { //Randomise order of individuals
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        std::uniform_int_distribution<int> chooseShuffle(individual, Population.size() - 1);
        int swap = chooseShuffle(rng);
        Individual tmp = Population[individual];
        Population[individual] = Population[swap];
        Population[swap] = tmp;
            }
}

constexpr double calcEnergy(const double local_s, const double x, const int i) {
    return exp(-local_s * pow(x + pow(-1, i), 2)); //Get feeding efficiency based on trait value
}

void getFood(std::vector<Individual> &Population, Habitat &Habitat) {
    for (int resource = 0; resource < static_cast<int>(Habitat.Ind.size()); ++resource) {
        double Sum = 0.0; //Create variable to keep track of total sum of feeding efficiencies
        for (int individual = 0; individual < static_cast<int>(Habitat.Ind[resource].size()); ++individual) {
            Sum += calcEnergy(s, Population[Habitat.Ind[resource][individual]].FeedEff, resource); //Add each relevant individual's feeding efficiency to variable
        }
        for (int individual = 0; individual < static_cast<int>(Habitat.Ind[resource].size()); ++individual) {
            Population[Habitat.Ind[resource][individual]].Food += calcEnergy(s, Population[Habitat.Ind[resource][individual]].FeedEff, resource) * Habitat.Resources[resource].Size / (Sum + pow(delta, -1) - 1); //Calculate found food for each individual
        }
    }
}

int makeChoice(std::vector<Individual> &Population, int i, std::vector<Habitat> &Habitats) {
    int l; //Variable which will output the decision that's made
    if (beta == 0) { //If individual chooses randomly (needed to avoid dividing by 0)
        std::uniform_int_distribution<int> chooseResource(0, 1);
        l = chooseResource(rng); //Set variable to either 0 or 1
    }
    else { //If individual has some degree of optimal choice
        std::vector<double> Fitness = Habitats[Population[i].Habitat].SumFeedEff; //Create vector of expected fitnesses at resources
        std::vector<double> Energy = Fitness; //Create vector of noisy assessments of fitnesses which choice will be based on
        for (int resource = 0; resource < static_cast<int>(Fitness.size()); ++resource) {
            Fitness[resource] = Habitats[Population[i].Habitat].Resources[resource].Size * calcEnergy(s, Population[i].FeedEff, resource) / (calcEnergy(s, Population[i].FeedEff, resource) + Fitness[resource] + pow(delta, -1) - 1); //Calculate how much food individual would get if it fed on the resources
        }
        double a = std::abs(Fitness[0] - Fitness[1]); //Calculate absolute difference between expected fitnesses
        double b = (a + pow((1 - beta) * pow(a, 2), 0.5) / beta); //Calculate width of uniform distributions based on a and beta
        for (int resource = 0; resource < static_cast<int>(Energy.size()); ++resource) {
            std::uniform_real_distribution<double> getAssessment(Fitness[resource] - b / 2, Fitness[resource] + b / 2);
            Energy[resource] = getAssessment(rng); //Get noisy assessment
        }
        if (Energy[0] > Energy[1]) { //If first resource is perceived to be more advantageous
            l = 0; //Choose first resource
        } else if (Energy[0] == Energy[1]) { //If resources are perceived to be equally good
            l = Population[i].FeedEff < 0.0 ? 0 : 1; //Choose resource the individual is better adapted to
        } else { //If second resource is perceived to be more advantageous
            l = 1; //Choose second resource
        }
    }
    return l; //Output the made decision
}

void updateValues(std::vector<Individual> &Population, Habitat &Habitat) {
    for (int resource = 0; resource < static_cast<int>(Habitat.Resources.size()); ++resource) {
        double TempSum = 0.0; //Create variable to keep track of sum of trait values
        for (int individual = 0; individual < static_cast<int>(Habitat.Ind[resource].size()); ++individual) {
            Habitat.Resources[resource].Sum += Population[Habitat.Ind[resource][individual]].FeedEff; //Update sum of trait values at resources
            TempSum += calcEnergy(s, Population[Habitat.Ind[resource][individual]].FeedEff, resource); //Add individuals' trait values to variable
        }
        Habitat.Resources[resource].FoundFood += TempSum / (TempSum + pow(delta, -1) - 1); //Calculate total amount of resource found during feeding round
        Habitat.Resources[resource].Indiv += Habitat.Ind[resource].size(); //Update total number of individuals feeding on resource
    }
}

void chooseResource(std::vector<Individual> &Population) {
    std::vector<Habitat> Habitats = createHabitats(); //Reset habitats
    for (int round = 0; round < d; ++round) {
        shuffle(Population); //Randomise the order of the individuals
        for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
            int j = makeChoice(Population, individual, Habitats); //Let individual make choice
            Habitats[Population[individual].Habitat].Ind[j].push_back(individual); //Add individual to list of individuals feeding on the chosen resource
            Habitats[Population[individual].Habitat].SumFeedEff[j] += calcEnergy(s, Population[individual].FeedEff, j); //Add individual's feeding efficiency to total feeding efficiencies at resource
        }
        for (int habitat = 0; habitat < static_cast<int>(Habitats.size()); ++habitat) {
            updateValues(Population, Habitats[habitat]); //Update values of variables
            getFood(Population, Habitats[habitat]); //Distribute found resources over individuals
            for (int resource = 0; resource < static_cast<int>(Habitats[habitat].Ind.size()); ++resource) {
                Habitats[habitat].Ind[resource].clear(); //Empty list of individuals feeding on the resources
                Habitats[habitat].SumFeedEff[resource] = 0.0; //Reset totals of feeding efficiencies at the resources
            }
        }
    }

    ofs << std::fixed << std::setprecision(6)
        << Habitats[0].Resources[0].Indiv / (Habitats[0].Resources[0].Indiv + Habitats[0].Resources[1].Indiv) << ","
        << Habitats[0].Resources[0].Sum / Habitats[0].Resources[0].Indiv << ","
        << Habitats[0].Resources[0].FoundFood / d << ","
        << Habitats[0].Resources[1].Indiv / (Habitats[0].Resources[0].Indiv + Habitats[0].Resources[1].Indiv) << ","
        << Habitats[0].Resources[1].Sum / Habitats[0].Resources[1].Indiv << ","
        << Habitats[0].Resources[1].FoundFood / d << ","
        << Habitats[1].Resources[0].Indiv / (Habitats[1].Resources[0].Indiv + Habitats[1].Resources[1].Indiv) << ","
        << Habitats[1].Resources[0].Sum / Habitats[1].Resources[0].Indiv << ","
        << Habitats[1].Resources[0].FoundFood / d << ","
        << Habitats[1].Resources[1].Indiv / (Habitats[1].Resources[0].Indiv + Habitats[1].Resources[1].Indiv) << ","
        << Habitats[1].Resources[1].Sum / Habitats[1].Resources[1].Indiv << ","
        << Habitats[1].Resources[1].FoundFood / d << "\n"; //Output info about habitats/resources

}

std::vector<double> getFitness(std::vector<Individual> &Population) {
    std::vector<double> Fitness; //Create list of fitnesses
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        Fitness.push_back(Population[individual].Food); //Add each individual's found food to fitness list
    }
    return Fitness;
}

void reproduce(std::vector<Individual> &Population, std::vector<double> &Fitness) {
    std::vector<Individual> NewPopulation; //Create new population
    std::discrete_distribution<int> chooseParent(Fitness.begin(), Fitness.end());
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        int p = chooseParent(rng); //Choose which individual reproduces based on fitness values
        NewPopulation.push_back(Population[p]); //Place copy of reproducing individual in new population
        NewPopulation.back().Food = 0.0; //Set found food of new individual to 0
    }
    Population = NewPopulation; //Replace old population with new population
}

void mutate(std::vector<Individual> &Population) {
    std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        double v = chooseFraction(rng); //Choose random number between 0 and 1; if it's smaller than mu, mutation will take place
        if (v < mu) { //if mutation takes place
            std::normal_distribution<double> chooseMutation(Population[individual].FeedEff, sigma);
            Population[individual].FeedEff = chooseMutation(rng); //Set new trait value to a small deviation from the old trait value taken from a normal distribution
            if (Population[individual].FeedEff < -1.0) {
                Population[individual].FeedEff = -1.0; //Make sure trait value is at least -1
            }
            else if (Population[individual].FeedEff > 1.0) {
                Population[individual].FeedEff = 1.0; //Make sure trait value is at most 1
            }
        }
    }
}

void migrate(std::vector<Individual> &Population) {
    std::vector<std::vector<int> > Ind = {{}, {}}; //Create list for individuals per habitat
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        Ind[Population[individual].Habitat].push_back(individual); //Put individual in list of habitat that it resides in
    }
    for (int habitat = 0; habitat < static_cast<int>(Ind.size()); ++habitat) {
        std::binomial_distribution<int> chooseMigrations(Ind[habitat].size(), m);
        int migrants = chooseMigrations(rng); //Choose number of migrating individuals based on migration rate and number of individuals in a habitat
        for (int migrant = 0; migrant < migrants; ++migrant) {
            std::uniform_int_distribution<int> chooseIndividual(0, Ind[habitat].size() - 1);
            int j = chooseIndividual(rng); //Choose which individual migrates
            Population[Ind[habitat][j]].Habitat = Population[Ind[habitat][j]].Habitat == 0 ? 1 : 0; //Change habitat the migrating individual resides in
            Ind[habitat].erase(Ind[habitat].begin() + j); //Remove individual from list of individuals in it previous habitat
        }
    }
}

bool sortPop(Individual i, Individual j) {
    return (i.FeedEff > j.FeedEff); //Helps sort the individuals by trait value to make the analysis easier
}

void simulate(std::vector<Individual> &Population) {
    ofs2 << "0";
    ofs3 << "0";
    sort(Population.begin(), Population.end(), sortPop);
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        ofs2 << "," << Population[individual].FeedEff;
        ofs3 << "," << Population[individual].Habitat;
    }
    ofs2 << "\n";
    ofs3 << "\n";
    for (int t = 0; t < g; ++t) {
        ofs << t << ",";
        migrate(Population);
        chooseResource(Population);
        std::vector<double> Fitness = getFitness(Population);
        reproduce(Population, Fitness);
        mutate(Population);
        if ((t+1) % save == 0) {
            ofs2 << t + 1;
            ofs3 << t + 1;
            sort(Population.begin(), Population.end(), sortPop);
            for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
                ofs2 << "," << Population[individual].FeedEff;
                ofs3 << "," << Population[individual].Habitat;
            }
            ofs2 << "\n";
            ofs3 << "\n";
        }
        std::cout << "Finished generation " << t+1 << "\n";
    }
}

int main(int argc, char* argv[]) {
    if(argc > 1) {
        if(argc == 6) {
            sscanf(argv[1], "%lf", &beta);
            sscanf(argv[2], "%lf", &s);
            sscanf(argv[3], "%lf", &delta);
            sscanf(argv[4], "%lf", &q);
            sscanf(argv[5], "%d", &seed);
        }
        else {
            return 1;
        }
    }
    rng.seed(seed);
    std::cout << "β = " << beta << " | s = " << s << " | δ = " << delta << " | q = " << q << "\n";
    std::cout << "Using seed " << seed << "\n";
    std::vector<Individual> Population = createPopulation();
    if (!ofs.is_open()) {
        return 1;
    }
    if (!ofs2.is_open()) {
        return 1;
    }
    ofs << "Time,Fraction H0R0,MeanX H0R0,Found H0R0,Fraction H0R1,MeanX H0R1,Found H0R1,Fraction H1R0,MeanX H1R0,Found H1R0,Fraction H1R1,MeanX H1R1,Found H1R1\n";
    ofs2 << "Time";
    ofs3 << "Time";
    for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
        ofs2 << ",Individual " << i;
        ofs3 << ",Individual " << i;
    }
    ofs2 << "\n";
    ofs3 << "\n";
    simulate(Population);
    return 0;
}