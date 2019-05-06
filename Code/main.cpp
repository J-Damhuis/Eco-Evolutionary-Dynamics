#include <iostream>
#include <vector>
#include <random>
#include <fstream>
#include <algorithm>

const int n = 1000; //Number of individuals
const int g = 100; //Number of generations
const int r = 2; //Number of resources
const int d = 10; //Number of feeding rounds per generation
const double mu = 0.5; //Mutation rate
const double sigma = 0.01; //Standard deviation of Gaussian distribution the size of a mutation is taken from
double beta = 1.0; //Degree of optimal choice
double s = 1.0; //Selection coefficient
double delta = 0.2; //Slope of Michaelis-Menten function for resource acquisition
int seed = 1;
std::vector<double> R = {10.0, 10.0}; //Size of resources
std::vector<std::vector<int> > Ind = {{}, {}}; //Indices of individuals at each resource
std::ofstream ofs("test.csv");
std::ofstream ofs2("heatmap.csv");
std::mt19937_64 rng;

class Individual {
public:
    double FeedEff;
    double Food;
};

class Resource {
public:
    double Sum;
    double Indiv;
    double FoundFood;
};

std::vector<Individual> createPopulation() {
    std::vector<Individual> Population(n);
    std::uniform_real_distribution<double> chooseValue(-1.0, -0.8);
    for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
        Population[i].FeedEff = chooseValue(rng);
        Population[i].Food = 0.0;
    }
    return Population;
}

std::vector<Resource> createResources() {
    std::vector<Resource> Resources(r);
    for (int j = 0; j < static_cast<int>(Resources.size()); ++j) {
        Resources[j].Sum = 0.0;
        Resources[j].FoundFood = 0.0;
        Resources[j].Indiv = 0;
    }
    return Resources;
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

void getFood(std::vector<Individual> &Population) {
    for (int j = 0; j < static_cast<int>(Ind.size()); ++j) {
        double Sum = 0.0;
        for (int i = 0; i < static_cast<int>(Ind[j].size()); ++i) {
            Sum += calcEnergy(s, Population[Ind[j][i]].FeedEff, j);
        }
        for (int i = 0; i < static_cast<int>(Ind[j].size()); ++i) {
            Population[Ind[j][i]].Food += calcEnergy(s, Population[Ind[j][i]].FeedEff, j) * R[j] / (Sum + pow(delta, -1) - 1);
        }
    }
}

int makeChoice(double v, std::vector<Individual> &Population, int i) {
    int l;
    if (v > beta) {
        std::uniform_int_distribution<int> chooseResource(0, 1);
        l = chooseResource(rng);
    }
    else {
        std::vector<double> Energy = {0.0, 0.0};
        for (int j = 0; j < static_cast<int>(Energy.size()); ++j) {
            for (int k = 0; k < static_cast<int>(Ind[j].size()); ++k) {
                Energy[j] += calcEnergy(s, Population[Ind[j][k]].FeedEff, j);
            }
            Energy[j] += calcEnergy(s, Population[i].FeedEff, j);
            Energy[j] = R[j] * calcEnergy(s, Population[i].FeedEff, j) / (Energy[j] + pow(delta, -1) - 1);
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

void updateValues(std::vector<Individual> &Population, std::vector<Resource> &Resources, int j) {
    std::vector<double> TempSum(r);
    for (int k = 0; k < static_cast<int>(Ind[j].size()); ++k) {
        Resources[j].Sum += Population[Ind[j][k]].FeedEff;
        TempSum[j] += calcEnergy(s, Population[Ind[j][k]].FeedEff, j);
        Resources[j].Indiv += 1.0;
    }
    Resources[j].FoundFood += TempSum[j] / (TempSum[j] + pow(delta, -1) - 1);
}

void chooseResource(std::vector<Individual> &Population) {
    std::vector<Resource> Resources = createResources();
    for (int e = 0; e < d; ++e) {
        shuffle(Population);
        for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
            std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
            int j = makeChoice(chooseFraction(rng), Population, i);
            Ind[j].push_back(i);
        }
        for (int j = 0; j < static_cast<int>(Resources.size()); ++j) {
            updateValues(Population, Resources, j);
        }
        getFood(Population);
        for (int j = 0; j < static_cast<int>(Ind.size()); ++j) {
            Ind[j].clear();
        }
    }
    ofs << Resources[0].Indiv/(Population.size()*d) << "," << Resources[0].Sum/Resources[0].Indiv << ","
        << Resources[0].FoundFood/d << "," << Resources[1].Indiv/(Population.size()*d) << ","
        << Resources[1].Sum/Resources[1].Indiv << "," << Resources[1].FoundFood/d << "\n";
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
        std::vector<double> Fitness = getFitness(Population);
        reproduce(Population, Fitness);
        mutate(Population);
        ofs2 << t+1;
        sort(Population.begin(), Population.end(), sortPop);
        for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
            ofs2 << "," << Population[i].FeedEff;
        }
        ofs2 << "\n";
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
    ofs << "Time,Resource 1,Resource 1,Resource 1,Resource 2,Resource 2,Resource 2\n";
    ofs2 << "Time";
    for (int i = 0; i < static_cast<int>(Population.size()); ++i) {
        ofs2 << ",Individual " << i;
    }
    ofs2 << "\n";
    simulate(Population);
    return 0;
}