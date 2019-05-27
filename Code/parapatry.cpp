#include <iostream>
#include <vector>
#include <random>
#include <fstream>
#include <algorithm>

const int n = 1000; //Number of individuals
const int g = 3000; //Number of generations
const int h = 2; //Number of habitats
const int r = 2; //Number of resources
const int d = 10; //Number of feeding rounds per generation
const double mu = 0.5; //Mutation rate
const double sigma = 0.01; //Standard deviation of Gaussian distribution the size of a mutation is taken from
const double m = 0.1; //Migration rate
double beta = 0.0; //Degree of optimal choice
double s = 1.0; //Selection coefficient
double delta = 0.01; //Slope of Michaelis-Menten function for resource acquisition
double q = 1.0; //Habitat asymmetry
int seed = 1;
int save = 1; //Save feeding efficiencies to file every so many generations
const std::vector<double> R = {10.0, 10.0}; //Total size of resources
std::ofstream ofs("test.csv");
std::ofstream ofs2("heatmap.csv");
//std::ofstream ofs3("fitness.csv");
std::mt19937_64 rng;

class Individual {
public:
    double FeedEff;
    double Food;
    int Habitat;
};

class Resource {
public:
    double Size;
    double Sum;
    double Indiv;
    double FoundFood;
};

class Habitat {
public:
    std::vector<Resource> Resources;
    std::vector<double> SumFeedEff;
    std::vector<std::vector<int> > Ind;
};

std::vector<Individual> createPopulation() {
    std::vector<Individual> Population(n);
    std::uniform_real_distribution<double> chooseValue(-1.0, -0.8);
    std::uniform_int_distribution<int> chooseHabitat(0, 1);
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        Population[individual].FeedEff = chooseValue(rng);
        Population[individual].Food = 0.0;
        Population[individual].Habitat = chooseHabitat(rng);
    }
    return Population;
}

std::vector<Habitat> createHabitats() {
    std::vector<Habitat> Habitats(h);
    for (int habitat = 0; habitat < static_cast<int>(Habitats.size()); ++habitat) {
        std::vector<Resource> Resources(r);
        for (int resource = 0; resource < static_cast<int>(Resources.size()); ++resource) {
            Resources[resource].Size = habitat == resource ? R[resource] * 0.5 * (1 + q) : R[resource] * 0.5 * (1 - q);
            Resources[resource].Sum = 0.0;
            Resources[resource].FoundFood = 0.0;
            Resources[resource].Indiv = 0;
        }
        Habitats[habitat].Resources = Resources;
        Habitats[habitat].SumFeedEff = {0.0, 0.0};
        Habitats[habitat].Ind = {{}, {}};
    }
    return Habitats;
}

void shuffle(std::vector<Individual> &Population) {
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        std::uniform_int_distribution<int> chooseShuffle(individual, Population.size() - 1);
        int swap = chooseShuffle(rng);
        Individual tmp = Population[individual];
        Population[individual] = Population[swap];
        Population[swap] = tmp;
    }
}

constexpr double calcEnergy(const double local_s, const double x, const int i) {
    return exp(-local_s * pow(x + pow(-1, i), 2));
}

void getFood(std::vector<Individual> &Population, Habitat &Habitat) {
    for (int resource = 0; resource < static_cast<int>(Habitat.Ind.size()); ++resource) {
        double Sum = 0.0;
        for (int individual = 0; individual < static_cast<int>(Habitat.Ind[resource].size()); ++individual) {
            Sum += calcEnergy(s, Population[Habitat.Ind[resource][individual]].FeedEff, resource);
        }
        for (int individual = 0; individual < static_cast<int>(Habitat.Ind[resource].size()); ++individual) {
            Population[Habitat.Ind[resource][individual]].Food += calcEnergy(s, Population[Habitat.Ind[resource][individual]].FeedEff, resource) * Habitat.Resources[resource].Size / (Sum + pow(delta, -1) - 1);
        }
    }
}

int makeChoice(double v, std::vector<Individual> &Population, int i, std::vector<Habitat> &Habitats) {
    int l;
    if (v > beta) {
        std::uniform_int_distribution<int> chooseResource(0, 1);
        l = chooseResource(rng);
    }
    else {
        std::vector<double> Energy = Habitats[Population[i].Habitat].SumFeedEff;
        for (int resource = 0; resource < static_cast<int>(Energy.size()); ++resource) {
            Energy[resource] = Habitats[Population[i].Habitat].Resources[resource].Size * calcEnergy(s, Population[i].FeedEff, resource) / (calcEnergy(s, Population[i].FeedEff, resource) + Energy[resource] + pow(delta, -1) - 1);
        }
        if (Energy[0] > Energy[1]) {
            l = 0;
        } else if (Energy[0] == Energy[1]) {
            l = Population[i].FeedEff < 0.0 ? 0 : 1;
        } else {
            l = 1;
        }
    }
    return l;
}

void updateValues(std::vector<Individual> &Population, Habitat &Habitat) {
    for (int resource = 0; resource < static_cast<int>(Habitat.Resources.size()); ++resource) {
        double TempSum = 0.0;
        double TempSum2 = 0.0;
        for (int individual = 0; individual < static_cast<int>(Habitat.Ind[resource].size()); ++individual) {
            TempSum2 += Population[Habitat.Ind[resource][individual]].FeedEff;
            TempSum += calcEnergy(s, Population[Habitat.Ind[resource][individual]].FeedEff, resource);
        }
        Habitat.Resources[resource].Sum += TempSum2 / Habitat.Ind[resource].size();
        Habitat.Resources[resource].FoundFood += TempSum / (TempSum + pow(delta, -1) - 1);
        Habitat.Resources[resource].Indiv += Habitat.Ind[resource].size();
    }
}

void chooseResource(std::vector<Individual> &Population) {
    std::vector<Habitat> Habitats = createHabitats();
    for (int round = 0; round < d; ++round) {
        shuffle(Population);
        for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
            std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
            int j = makeChoice(chooseFraction(rng), Population, individual, Habitats);
            Habitats[Population[individual].Habitat].Ind[j].push_back(individual);
            Habitats[Population[individual].Habitat].SumFeedEff[j] += calcEnergy(s, Population[individual].FeedEff, j);
        }
        for (int habitat = 0; habitat < static_cast<int>(Habitats.size()); ++habitat) {
            updateValues(Population, Habitats[habitat]);
            getFood(Population, Habitats[habitat]);
            for (int resource = 0; resource < static_cast<int>(Habitats[habitat].Ind.size()); ++resource) {
                Habitats[habitat].Ind[resource].clear();
                Habitats[habitat].SumFeedEff[resource] = 0.0;
            }
        }
    }

    ofs << Habitats[0].Resources[0].Indiv / (Habitats[0].Resources[0].Indiv + Habitats[0].Resources[1].Indiv) << ","
        << Habitats[0].Resources[0].Sum / d << ","
        << Habitats[0].Resources[0].FoundFood / d << ","
        << Habitats[0].Resources[1].Indiv / (Habitats[0].Resources[1].Indiv + Habitats[0].Resources[0].Indiv) << ","
        << Habitats[0].Resources[1].Sum / d << ","
        << Habitats[0].Resources[1].FoundFood / d << ","
        << Habitats[1].Resources[0].Indiv / (Habitats[1].Resources[0].Indiv + Habitats[1].Resources[1].Indiv) << ","
        << Habitats[1].Resources[0].Sum / d << ","
        << Habitats[1].Resources[0].FoundFood / d << ","
        << Habitats[1].Resources[1].Indiv / (Habitats[1].Resources[1].Indiv + Habitats[1].Resources[0].Indiv) << ","
        << Habitats[1].Resources[1].Sum / d << ","
        << Habitats[1].Resources[1].FoundFood / d << "\n";

}

std::vector<std::vector<double> > getFitness(std::vector<Individual> &Population) {
    std::vector<std::vector<double> > Fitness(h);
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        Fitness[Population[individual].Habitat].push_back(Population[individual].Food);
    }
    return Fitness;
}

void reproduce(std::vector<Individual> &Population, std::vector<std::vector<double> > &Fitness) {
    std::vector<std::vector<int> > Ind = {{}, {}};
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        Ind[Population[individual].Habitat].push_back(individual);
    }
    std::vector<Individual> NewPopulation;
    for (int habitat = 0; habitat < static_cast<int>(Fitness.size()); ++habitat) {
        std::discrete_distribution<int> chooseParent(Fitness[habitat].begin(), Fitness[habitat].end());
        for (int individual = 0; individual < static_cast<int>(Ind[habitat].size()); ++individual) {
            int p = chooseParent(rng);
            NewPopulation.push_back(Population[Ind[habitat][p]]);
            NewPopulation.back().Food = 0.0;
        }
    }
    Population = NewPopulation;
}

void mutate(std::vector<Individual> &Population) {
    std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        double v = chooseFraction(rng);
        if (v < mu) {
            std::normal_distribution<double> chooseMutation(Population[individual].FeedEff, sigma);
            Population[individual].FeedEff = chooseMutation(rng);
            if (Population[individual].FeedEff < -1.0) {
                Population[individual].FeedEff = -1.0;
            }
            else if (Population[individual].FeedEff > 1.0) {
                Population[individual].FeedEff = 1.0;
            }
        }
    }
}

void migrate(std::vector<Individual> &Population) {
    std::vector<std::vector<int> > Ind = {{}, {}};
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        Ind[Population[individual].Habitat].push_back(individual);
    }
    int k = Ind[0].size() < Ind[1].size() ? 0 : 1;
    std::binomial_distribution<int> chooseMigrations(Ind[k].size(), m);
    int migrants = chooseMigrations(rng);
    for (int migrant = 0; migrant < migrants; ++migrant) {
        std::uniform_int_distribution<int> chooseIndividual(0, Ind[k].size()-1);
        int j = chooseIndividual(rng);
        Population[Ind[0][j]].Habitat = 1;
        Population[Ind[1][j]].Habitat = 0;
        Ind[0].erase(Ind[0].begin()+j);
        Ind[1].erase(Ind[1].begin()+j);
    }
}

bool sortPop(Individual i, Individual j) {
    return (i.FeedEff > j.FeedEff);
}

void simulate(std::vector<Individual> &Population) {
    ofs2 << "0";
    sort(Population.begin(), Population.end(), sortPop);
    for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
        ofs2 << "," << Population[i].FeedEff;
    }
    ofs2 << "\n";
    for (int t = 0; t < g; ++t) {
        ofs << t << ",";
        chooseResource(Population);
/*
        ofs3 << t;
        for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
            ofs3 << "," << Population[i].Food;
        }
        ofs3 << "\n";
*/
        std::vector<std::vector<double> > Fitness = getFitness(Population);
        reproduce(Population, Fitness);
        mutate(Population);
        migrate(Population);
        if ((t+1) % save == 0) {
            ofs2 << t + 1;
            sort(Population.begin(), Population.end(), sortPop);
            for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
                ofs2 << "," << Population[i].FeedEff;
            }
            ofs2 << "\n";
        }
        std::cout << "Finished generation " << t+1 << "\n";
    }
}

int main(int argc, char* argv[]) {
    if(argc > 1) {
        if(argc == 5) {
            sscanf(argv[1], "%lf", &beta);
            sscanf(argv[2], "%lf", &s);
            sscanf(argv[3], "%lf", &delta);
            sscanf(argv[4], "%d", &seed);
        }
        else {
            return 1;
        }
    }
    rng.seed(seed);
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
    //ofs3 << "Time";
    for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
        ofs2 << ",Individual " << i;
        //ofs3 << ",Individual " << i;
    }
    ofs2 << "\n";
    //ofs3 << "\n";
    simulate(Population);
    return 0;
}