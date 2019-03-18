#include <iostream>
#include <vector>
#include <random>
#include <fstream>

const int n = 50;
const int t = 20;
const double mu = 0.5;
const double sigma = 0.01;
const double MaxR1 = 10.0;
const double MaxR2 = 10.0;
double R1 = MaxR1;
double R2 = MaxR2;
std::vector<int> IndR1;
std::vector<int> IndR2;
std::ofstream ofs("test.csv");
std::uniform_real_distribution<double> chooseValue(-1.0, 1.0);
std::mt19937_64 rng;

class Individual {
public:
    double FeedEff;
};

std::vector<Individual> createPopulation() {
    std::vector<Individual> Population(n);
    for (int i = 0; i < Population.size(); ++i) {
        Population[i].FeedEff = chooseValue(rng);
        //std::cout << "Individual " << i << "'s feeding efficiency is: " << Population[i].FeedEff << "\n";
    }
    return Population;
};

void shuffle(std::vector<Individual> &Population) {
    for (int i = Population.size() - 1; i >= 0; --i) {
        std::uniform_int_distribution<int> chooseShuffle(0, i);
        int j = chooseShuffle(rng);
        Individual tmp = Population[i];
        Population[i] = Population[j];
        Population[j] = tmp;
    }
    //std::cout << "Population has been shuffled\n";
}

double calcEnergyR1(double x) {
    return exp(-1 * pow(x + 1, 2));
}

double calcEnergyR2(double x) {
    return exp(-1 * pow(x - 1, 2));
}

void chooseResource(std::vector<Individual> &Population) {
    R1 = MaxR1;
    R2 = MaxR2;
    for (int i = 0; i < Population.size(); ++i) {
        double EnergyR1 = R1 / (IndR1.size() + 1) * calcEnergyR1(Population[i].FeedEff);
        double EnergyR2 = R2 / (IndR2.size() + 1) * calcEnergyR2(Population[i].FeedEff);
        if (EnergyR1 > EnergyR2) {
            IndR1.push_back(i);
            //std::cout << "Individual " << i << " has chosen resource 1\n";
        }
        else {
            IndR2.push_back(i);
            //std::cout << "Individual " << i << " has chosen resource 2\n";
        }
    }
}

std::vector<double> getFitness(std::vector<Individual> &Population) {
    double AvgR1 = 0.0;
    double AvgR2 = 0.0;
    std::vector<double> Fitness(Population.size());
    for (int i = 0; i < IndR1.size(); ++i) {
        Fitness[IndR1[i]] = R1 / IndR1.size() * calcEnergyR1(Population[IndR1[i]].FeedEff);
        AvgR1 += Population[IndR1[i]].FeedEff;
    }
    for (int i = 0; i < IndR2.size(); ++i) {
        Fitness[IndR2[i]] = R2 / IndR2.size() * calcEnergyR2(Population[IndR2[i]].FeedEff);
        AvgR2 += Population[IndR2[i]].FeedEff;
    }
    ofs << IndR1.size() << "," << AvgR1 / IndR1.size() << "," << IndR2.size() << "," << AvgR2 / IndR2.size() << "\n";
    IndR1.clear();
    IndR2.clear();
    /*for (int i = 0; i < Fitness.size(); ++i) {
        std::cout << "Individual " << i << " has a fitness of " << Fitness[i] << "\n";
    }*/
    return Fitness;
}

void reproduce(std::vector<Individual> &Population, std::vector<double> &Fitness) {
    std::vector<Individual> NewPopulation = Population;
    std::discrete_distribution<int> chooseParent(Fitness.begin(), Fitness.end());
    for (int i = 0; i < NewPopulation.size(); ++i) {
        int p = chooseParent(rng);
        NewPopulation[i] = Population[p];
        //std::cout << "Individual " << p << " reproduced\n";
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
            std::cout << "Individual " << i << "'s feeding efficiency mutated to be " << Population[i].FeedEff << "\n";
        }
    }
}

void simulate(std::vector<Individual> &Population) {
    for (int g = 0; g < t; ++g) {
        shuffle(Population);
        chooseResource(Population);
        std::vector<double> Fitness = getFitness(Population);
        reproduce(Population, Fitness);
        mutate(Population);
    }
}

int main() {

    std::vector<Individual> Population = createPopulation();
    if (!ofs.is_open()) {
        return 1;
    }
    ofs << "NrR1,AvgR1,NrR2,AvgR2\n";
    simulate(Population);

    return 0;
}