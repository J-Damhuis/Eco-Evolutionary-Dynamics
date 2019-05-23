#include <iostream>
#include <vector>
#include <random>
#include <fstream>
#include <algorithm>

const int n = 10; //Number of individuals
const int g = 3; //Number of generations
const int h = 2; //Number of habitats
const int r = 2; //Number of resources
const int d = 1; //Number of feeding rounds per generation
const double mu = 0.5; //Mutation rate
const double sigma = 0.01; //Standard deviation of Gaussian distribution the size of a mutation is taken from
const double m = 0.1; //Migration rate
double beta = 0.0; //Degree of optimal choice
double s = 1.0; //Selection coefficient
double delta = 0.01; //Slope of Michaelis-Menten function for resource acquisition
double q = 0.0; //Habitat asymmetry
int seed = 1;
//int save = 10; //Save feeding efficiencies to file every so many generations
const std::vector<double> R = {10.0, 10.0}; //Total size of resources
//std::ofstream ofs("test.csv");
//std::ofstream ofs2("heatmap.csv");
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
    for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
        Population[i].FeedEff = chooseValue(rng);
        Population[i].Food = 0.0;
        Population[i].Habitat = chooseHabitat(rng);
    }
    return Population;
}

std::vector<Habitat> createHabitats() {
    std::vector<Habitat> Habitats(h);
    for (int i = 0; i < static_cast<int>(Habitats.size()); ++i) {
        std::vector<Resource> Resources(r);
        for (int j = 0; j < static_cast<int>(Resources.size()); ++j) {
            Resources[j].Size = i == j ? R[j] * 0.5 * (1 + q) : R[j] * 0.5 * (1 - q);
            Resources[j].Sum = 0.0;
            Resources[j].FoundFood = 0.0;
            Resources[j].Indiv = 0;
        }
        Habitats[i].Resources = Resources;
        Habitats[i].SumFeedEff = {0.0, 0.0};
        Habitats[i].Ind = {{}, {}};
    }
    return Habitats;
}

void shuffle(std::vector<Individual> &Population) {
    for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
        std::uniform_int_distribution<int> chooseShuffle(i, Population.size() - 1);
        int j = chooseShuffle(rng);
        Individual tmp = Population[i];
        Population[i] = Population[j];
        Population[j] = tmp;
    }
}

constexpr double calcEnergy(const double local_s, const double x, const int i) {
    return exp(-local_s * pow(x + pow(-1, i), 2));
}

void getFood(std::vector<Individual> &Population, Habitat &Habitat) {
    for (int j = 0; j < static_cast<int>(Habitat.Ind.size()); ++j) {
        double Sum = 0.0;
        for (int i = 0; i < static_cast<int>(Habitat.Ind[j].size()); ++i) {
            Sum += calcEnergy(s, Population[Habitat.Ind[j][i]].FeedEff, j);
        }
        for (int i = 0; i < static_cast<int>(Habitat.Ind[j].size()); ++i) {
            Population[Habitat.Ind[j][i]].Food += calcEnergy(s, Population[Habitat.Ind[j][i]].FeedEff, j) * Habitat.Resources[j].Size / (Sum + pow(delta, -1) - 1);
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
        for (int j = 0; j < static_cast<int>(Energy.size()); ++j) {
            Energy[j] = Habitats[Population[i].Habitat].Resources[j].Size * calcEnergy(s, Population[i].FeedEff, j) / (calcEnergy(s, Population[i].FeedEff, j) + Energy[j] + pow(delta, -1) - 1);
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
    for (int j = 0; j < static_cast<int>(Habitat.Resources.size()); ++j) {
        std::vector<double> TempSum(r);
        for (int k = 0; k < static_cast<int>(Habitat.Ind[j].size()); ++k) {
            Habitat.Resources[j].Sum += Population[Habitat.Ind[j][k]].FeedEff;
            TempSum[j] += calcEnergy(s, Population[Habitat.Ind[j][k]].FeedEff, j);
            Habitat.Resources[j].Indiv += 1.0;
        }
        Habitat.Resources[j].FoundFood += TempSum[j] / (TempSum[j] + pow(delta, -1) - 1);
    }
}

void chooseResource(std::vector<Individual> &Population) {
    std::vector<Habitat> Habitats = createHabitats();
    for (int e = 0; e < d; ++e) {
        shuffle(Population);
        for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
            std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
            int j = makeChoice(chooseFraction(rng), Population, i, Habitats);
            Habitats[Population[i].Habitat].Ind[j].push_back(i);
            Habitats[Population[i].Habitat].SumFeedEff[j] += calcEnergy(s, Population[i].FeedEff, j);
        }
        for (int i = 0; i < static_cast<int>(Habitats.size()); ++i) {
            updateValues(Population, Habitats[i]);
            getFood(Population, Habitats[i]);
            for (int j = 0; j < static_cast<int>(Habitats[i].Ind.size()); ++j) {
                Habitats[i].Ind[j].clear();
                Habitats[i].SumFeedEff[j] = 0.0;
            }
        }
    }
    /*ofs << Resources[0].Indiv/(Population.size()*d) << "," << Resources[0].Sum/Resources[0].Indiv << ","
        << Resources[0].FoundFood/d << "," << Resources[1].Indiv/(Population.size()*d) << ","
        << Resources[1].Sum/Resources[1].Indiv << "," << Resources[1].FoundFood/d << "\n";*/
}

std::vector<double> getFitness(std::vector<Individual> &Population) {
    std::vector<double> Fitness(Population.size());
    for (int i = 0; i < static_cast<int>(Fitness.size()); ++i) {
        Fitness[i] = Population[i].Food;
    }
    return Fitness;
}

void reproduce(std::vector<Individual> &Population, std::vector<double> &Fitness) {
    std::vector<Individual> NewPopulation = Population;
    std::discrete_distribution<int> chooseParent(Fitness.begin(), Fitness.end());
    for (int i = 0; i < static_cast<int>(NewPopulation.size()); ++i) {
        int p = chooseParent(rng);
        Population[i] = NewPopulation[p];
        Population[i].Food = 0.0;
    }
}

void mutate(std::vector<Individual> &Population) {
    std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
    for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
        double v = chooseFraction(rng);
        if (v < mu) {
            std::normal_distribution<double> chooseMutation(Population[i].FeedEff, sigma);
            Population[i].FeedEff = chooseMutation(rng);
            if (Population[i].FeedEff < -1.0) {
                Population[i].FeedEff = -1.0;
            }
            else if (Population[i].FeedEff > 1.0) {
                Population[i].FeedEff = 1.0;
            }
        }
    }
}

void migrate(std::vector<Individual> &Population) {
    std::vector<std::vector<int> > Ind = {{}, {}};
    for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
        Ind[Population[i].Habitat].push_back(i);
    }
    std::binomial_distribution<int> chooseMigrations(n/2, m);
    int migrants = chooseMigrations(rng);
    std::uniform_int_distribution<int> chooseIndividual(0, Population.size() / 2 - 1);
    for (int i = 0; i < migrants; ++i) {
        int j = chooseIndividual(rng);
        Population[Ind[0][j]].Habitat = 1;
        Population[Ind[1][j]].Habitat = 0;
    }
}

bool sortPop(Individual i, Individual j) {
    return (i.FeedEff > j.FeedEff);
}

void simulate(std::vector<Individual> &Population) {
    //ofs2 << "0";
    sort(Population.begin(), Population.end(), sortPop);
    /*for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
        ofs2 << "," << Population[i].FeedEff;
    }
    ofs2 << "\n";*/
    for (int t = 0; t < g; ++t) {
        //ofs << t << ",";
        chooseResource(Population);
/*
        ofs3 << t;
        for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
            ofs3 << "," << Population[i].Food;
        }
        ofs3 << "\n";
*/
        std::vector<double> Fitness = getFitness(Population);
        reproduce(Population, Fitness);
        mutate(Population);
        migrate(Population);
        /*if ((t+1) % save == 0) {
            ofs2 << t + 1;
            sort(Population.begin(), Population.end(), sortPop);
            for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
                ofs2 << "," << Population[i].FeedEff;
            }
            ofs2 << "\n";
        }*/
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
    /*if (!ofs.is_open()) {
        return 1;
    }
    if (!ofs2.is_open()) {
        return 1;
    }
    ofs << "Time,Resource 1,Resource 1,Resource 1,Resource 2,Resource 2,Resource 2\n";
    ofs2 << "Time";
    //ofs3 << "Time";
    for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
        ofs2 << ",Individual " << i;
        //ofs3 << ",Individual " << i;
    }
    ofs2 << "\n";*/
    //ofs3 << "\n";
    simulate(Population);
    return 0;
}