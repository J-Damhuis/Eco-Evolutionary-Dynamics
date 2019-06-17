#include <iostream>
#include <vector>
#include <random>
#include <fstream>
#include <algorithm>
#include <iomanip>

const int n = 1000; //Number of individuals
const int g = 3000; //Number of generations
const int h = 2; //Number of habitats
const int r = 2; //Number of resources
const int d = 10; //Number of feeding rounds per generation
const double mu = 0.5; //Mutation rate
const double sigma = 0.01; //Standard deviation of Gaussian distribution the size of a mutation is taken from
const double m = 0.1; //Migration rate
double beta = 0.0; //Degree of optimal choice
double s = 0.6; //Selection coefficient
double delta = 0.01; //Slope of Michaelis-Menten function for resource acquisition
double q = 0.0; //Habitat asymmetry
int seed = 1;
const int save = 1; //Save feeding efficiencies to file every so many generations
const std::vector<double> R = {10.0, 10.0}; //Total size of resources
std::ofstream ofs("test.csv");
std::ofstream ofs2("heatmap.csv");
std::ofstream ofs3("habitat.csv");
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
    //std::cout << "Initial values individuals\n";
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        Population[individual].FeedEff = chooseValue(rng);
        Population[individual].Food = 0.0;
        Population[individual].Habitat = 0;
        //std::cout << individual << ":\t" << Population[individual].FeedEff << "\t" << Population[individual].Habitat << "\n";
    }
    //std::cout << "\n";
    return Population;
}

std::vector<Habitat> createHabitats() {
    std::vector<Habitat> Habitats(h);
    //std::cout << "Resource sizes in habitats\n";
    for (int habitat = 0; habitat < static_cast<int>(Habitats.size()); ++habitat) {
        std::vector<Resource> Resources(r);
        for (int resource = 0; resource < static_cast<int>(Resources.size()); ++resource) {
            Resources[resource].Size = habitat == resource ? R[resource] * (1.0 / h) * (1.0 + q) : R[resource] * (1.0 / h) * (1.0 - q);
            Resources[resource].Sum = 0.0;
            Resources[resource].FoundFood = 0.0;
            Resources[resource].Indiv = 0;
            //std::cout << "Habitat " << habitat << ":\tResource " << resource << ":\t" << Resources[resource].Size << "\n";
        }
        Habitats[habitat].Resources = Resources;
        Habitats[habitat].SumFeedEff = {0.0, 0.0};
        Habitats[habitat].Ind = {{}, {}};
    }
    //std::cout << "\n";
    return Habitats;
}

void shuffle(std::vector<Individual> &Population) {
    //std::cout << "Shuffles\n";
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        std::uniform_int_distribution<int> chooseShuffle(individual, Population.size() - 1);
        int swap = chooseShuffle(rng);
        Individual tmp = Population[individual];
        Population[individual] = Population[swap];
        Population[swap] = tmp;
        //std::cout << individual << " <-> " << swap << "\n";
    }
    //std::cout << "\n";
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
            //std::cout << Habitat.Ind[resource][individual] << ":\t" << Population[Habitat.Ind[resource][individual]].Food << "\n";
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
    //std::cout << i << ":\t" << l << "\t(" << Population[i].FeedEff << ")\n";
    return l;
}

void updateValues(std::vector<Individual> &Population, Habitat &Habitat) {
    for (int resource = 0; resource < static_cast<int>(Habitat.Resources.size()); ++resource) {
        double TempSum = 0.0;
        for (int individual = 0; individual < static_cast<int>(Habitat.Ind[resource].size()); ++individual) {
            Habitat.Resources[resource].Sum += Population[Habitat.Ind[resource][individual]].FeedEff;
            TempSum += calcEnergy(s, Population[Habitat.Ind[resource][individual]].FeedEff, resource);
        }
        Habitat.Resources[resource].FoundFood += TempSum / (TempSum + pow(delta, -1) - 1);
        Habitat.Resources[resource].Indiv += Habitat.Ind[resource].size();
    }
}

void chooseResource(std::vector<Individual> &Population) {
    std::vector<Habitat> Habitats = createHabitats();
    for (int round = 0; round < d; ++round) {
        shuffle(Population);
        //std::cout << "Choices made\n";
        for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
            std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
            int j = makeChoice(chooseFraction(rng), Population, individual, Habitats);
            Habitats[Population[individual].Habitat].Ind[j].push_back(individual);
            Habitats[Population[individual].Habitat].SumFeedEff[j] += calcEnergy(s, Population[individual].FeedEff, j);
        }
        //std::cout << "\n";
        //std::cout << "Food found\n";
        for (int habitat = 0; habitat < static_cast<int>(Habitats.size()); ++habitat) {
            updateValues(Population, Habitats[habitat]);
            getFood(Population, Habitats[habitat]);
            for (int resource = 0; resource < static_cast<int>(Habitats[habitat].Ind.size()); ++resource) {
                Habitats[habitat].Ind[resource].clear();
                Habitats[habitat].SumFeedEff[resource] = 0.0;
            }
        }
        //std::cout << "\n";
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
        << Habitats[1].Resources[1].FoundFood / d << "\n";

}

std::vector<double> getFitness(std::vector<Individual> &Population) {
    std::vector<double> Fitness;
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        Fitness.push_back(Population[individual].Food);
    }
    return Fitness;
}

void reproduce(std::vector<Individual> &Population, std::vector<double> &Fitness) {
    std::vector<Individual> NewPopulation;
    //std::cout << "Individuals that reproduce\n";
    std::discrete_distribution<int> chooseParent(Fitness.begin(), Fitness.end());
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        int p = chooseParent(rng);
        NewPopulation.push_back(Population[p]);
        NewPopulation.back().Food = 0.0;
        //std::cout << p << "\n";
    }
    //std::cout << "\n";
    Population = NewPopulation;
}

void mutate(std::vector<Individual> &Population) {
    std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
    //std::cout << "Feeding efficiencies after mutation\n";
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
            //std::cout << individual << ":\t" << Population[individual].FeedEff << "\n";
        }
    }
    //std::cout << "\n";
}

void migrate(std::vector<Individual> &Population) {
    std::vector<std::vector<int> > Ind = {{}, {}};
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        Ind[Population[individual].Habitat].push_back(individual);
    }
    //std::cout << "Migrations: \n";
    for (int habitat = 0; habitat < static_cast<int>(Ind.size()); ++habitat) {
        std::binomial_distribution<int> chooseMigrations(Ind[habitat].size(), m);
        int migrants = chooseMigrations(rng);
        //std::cout << "Habitat " << habitat << "\n";
        for (int migrant = 0; migrant < migrants; ++migrant) {
            std::uniform_int_distribution<int> chooseIndividual(0, Ind[habitat].size() - 1);
            int j = chooseIndividual(rng);
            //std::cout << "Chose number " << j << "(" << Population[Ind[habitat][j]].FeedEff << ")\n";
            Population[Ind[habitat][j]].Habitat = Population[Ind[habitat][j]].Habitat == 0 ? 1 : 0;
            Ind[habitat].erase(Ind[habitat].begin() + j);
        }
    }
    //std::cout << "\n";
}

bool sortPop(Individual i, Individual j) {
    return (i.FeedEff > j.FeedEff);
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