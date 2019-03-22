#include <iostream>
#include <vector>
#include <random>
#include <fstream>
#include <w32api/ntdef.h>

const int n = 1000;
const int g = 1000;
const int d = 10;
const double mu = 0.5;
const double sigma = 0.01;
const double beta = 1.0;
const double s = 1.0;
std::vector<double> MaxR = {10.0, 10.0};
std::vector<double> R = MaxR;
std::vector<std::vector<int> > Ind = {{}, {}};
std::ofstream ofs("test.csv");
std::ofstream ofs2("heatmap.csv");
std::mt19937_64 rng;

class Individual {
public:
    double FeedEff;
    double Food;
};

std::vector<Individual> createPopulation() {
    std::vector<Individual> Population(n);
    std::uniform_real_distribution<double> chooseValue(-1.0, -0.8);
    for (int i = 0; i < Population.size(); ++i) {
        Population[i].FeedEff = chooseValue(rng);
        Population[i].Food = 0.0;
        //std::cout << i << ": " << Population[i].FeedEff << "\n";
    }
    //std::cout << "\n";
    return Population;
};

void shuffle(std::vector<Individual> &Population) {
    for (int i = 0; i < Population.size(); ++i) {
        std::uniform_int_distribution<int> chooseShuffle(i, Population.size() - 1);
        int j = chooseShuffle(rng);
        Individual tmp = Population[i];
        Population[i] = Population[j];
        Population[j] = tmp;
        //std::cout << j << "\t";
    }
    //std::cout << "\n\n";
}

double calcEnergy(double x, int i) {
    return exp(-s * pow(x + pow(-1, i), 2));
}

void getFood(std::vector<Individual> &Population) {
    for (int j = 0; j < Ind.size(); ++j) {
        double Sum = 0.0;
        for (int i = 0; i < Ind[j].size(); ++i) {
            Sum += calcEnergy(Population[Ind[j][i]].FeedEff, j);
        }
        for (int i = 0; i < Ind[j].size(); ++i) {
            Population[Ind[j][i]].Food += R[j] * calcEnergy(Population[Ind[j][i]].FeedEff, j) / Sum;
            //std::cout << Ind[j][i] << ": " << Population[Ind[j][i]].Food << "\n";
        }
    }
    //std::cout << "\n";
}

void chooseResource(std::vector<Individual> &Population) {
    std::vector<double> Sum = {0.0, 0.0};
    std::vector<double> Indiv = {0, 0};
    for (int e = 0; e < d; ++e) {
        shuffle(Population);
        R = MaxR;
        for (int i = 0; i < Population.size(); ++i) {
            std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
            double r = chooseFraction(rng);
            if (r > beta) {
                double r2 = chooseFraction(rng);
                int j = r2 > 0.5 ? 0 : 1;
                Ind[j].push_back(i);
                Sum[j] += Population[i].FeedEff;
                Indiv[j] += 1.0;
                //std::cout << i << ": " << j << "\n";
            }
            else {
                std::vector<double> Energy = {0.0, 0.0};
                for (int j = 0; j < Energy.size(); ++j) {
                    for (int k = 0; k < Ind[j].size(); ++k) {
                        Energy[j] += calcEnergy(Population[Ind[j][k]].FeedEff, j);
                    }
                    Energy[j] += calcEnergy(Population[i].FeedEff, j);
                    Energy[j] = R[j] * calcEnergy(Population[i].FeedEff, j) / Energy[j];
                }
                int j;
                if (Energy[0] > Energy[1]) {
                    j = 0;
                } else if (Energy[0] == Energy[1]) {
                    j = Population[i].FeedEff < 0.0 ? 0 : 1;
                } else {
                    j = 1;
                }
                Ind[j].push_back(i);
                Sum[j] += Population[i].FeedEff;
                Indiv[j] += 1.0;
                //std::cout << i << ": " << j << "\n";
            }
        }
        //std::cout << "\n";
        getFood(Population);
        for (int i = 0; i < Ind.size(); ++i) {
            Ind[i].clear();
        }
    }
    ofs << Indiv[0]/(Population.size()*d) << "," << Sum[0]/Indiv[0] << ","
        << Indiv[1]/(Population.size()*d) << "," << Sum[1]/Indiv[1] << "\n";
}

std::vector<double> getFitness(std::vector<Individual> &Population) {
    std::vector<double> Fitness(Population.size());
    for (int i = 0; i < Fitness.size(); ++i) {
        Fitness[i] = Population[i].Food;
        //std::cout << i << ": " << Population[i].Food << "\n";
    }
    //std::cout << "\n";
    return Fitness;
}

void reproduce(std::vector<Individual> &Population, std::vector<double> &Fitness) {
    std::vector<Individual> NewPopulation = Population;
    std::discrete_distribution<int> chooseParent(Fitness.begin(), Fitness.end());
    for (int i = 0; i < NewPopulation.size(); ++i) {
        int p = chooseParent(rng);
        NewPopulation[i] = Population[p];
    }
    Population = NewPopulation;
}

void mutate(std::vector<Individual> &Population) {
    std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
    for (int i = 0; i < Population.size(); ++i) {
        double r = chooseFraction(rng);
        if (r < mu) {
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

void simulate(std::vector<Individual> &Population) {
    ofs2 << "0";
    for (int i = 0; i < Population.size(); ++i) {
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
        for (int i = 0; i < Population.size(); ++i) {
            ofs2 << "," << Population[i].FeedEff;
        }
        ofs2 << "\n";
        std::cout << "Finished generation " << t << "\n";
    }
}

int main() {
    std::vector<Individual> Population = createPopulation();
    if (!ofs.is_open()) {
        return 1;
    }
    if (!ofs2.is_open()) {
        return 1;
    }
    ofs << "Time,Resource 1,Resource 1,Resource 2,Resource 2\n";
    ofs2 << "Time";
    for (int i = 0; i < Population.size(); ++i) {
        ofs2 << ",Individual " << i;
    }
    ofs2 << "\n";
    simulate(Population);
    return 0;
}