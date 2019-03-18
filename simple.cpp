#include <iostream>
#include <random>
#include <vector>
#include <fstream>

const int n = 4;
const int t = 2;
const double mu = 0.5;
const double sigma = 0.01;
const double MaxR1 = 10.0;
const double MaxR2 = 10.0;
const double delta = 1.0;
double R1 = MaxR1;
double R2 = MaxR2;
std::ofstream ofs("test.csv");
std::uniform_real_distribution<double> chooseValue(-1.0, 1.0);
std::mt19937_64 rng;

class Individual {
public:
    double FeedEff;
    double Fitness;
    double Prob;
};

std::vector<Individual> createPopulation() {
    std::vector<Individual> Population(n);
    for (int i = 0; i < Population.size(); ++i) {
        //Population[i].FeedEff = chooseValue(rng);
        Population[i].FeedEff = pow(-1.0, i);
        Population[i].Fitness = 0.0;
        Population[i].Prob = 1.0;
    }
    return Population;
}

int chooseIndividual(std::vector<Individual> &Population) {
    std::vector<double> Weights(n);
    for (int i = 0; i < Population.size(); ++i) {
        Weights[i] = Population[i].Prob;
        //std::cout << "Individual " << i << " has weight " << Population[i].Prob << "\n";
    }
    std::discrete_distribution<int> chooseInd(Weights.begin(), Weights.end());
    //std::uniform_int_distribution<int> chooseInd(0, 49);
    int i = chooseInd(rng);
    return i;
}

int chooseResource() {
    if (R1 < delta && R2 < delta) {
        return 0;
    }
    else if (R1 < delta && R2 > delta) {
        return 2;
    }
    else if (R1 > delta && R2 < delta) {
        return 1;
    }
    else if (R1 > delta && R2 > delta) {
        std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
        double r = chooseFraction(rng);
        if (r > R1 / (R1 + R2)) {
            return 2;
        }
        else {
            return 1;
        }
    }
}

bool doesIndAccept(Individual i, int j) {
    if (i.FeedEff > 0 && j == 2) {
        return true;
    }
    else if (i.FeedEff < 0 && j == 1) {
        return true;
    }
    else if (i.FeedEff > 0 && j == 1) {
        return false;
    }
    else if (i.FeedEff < 0 && j == 2) {
        return false;
    }
}

double calcEnergyR1(double x) {
    return exp(-1 * pow(x + 1, 2));
}

double calcEnergyR2(double x) {
    return exp(-1 * pow(x - 1, 2));
}

void eat(std::vector<Individual> &Population, int &i, int &j) {
    double Energy = 0.0;
    if (j == 1) {
        Energy = delta * calcEnergyR1(Population[i].FeedEff);
        R1 -= Energy;
    }
    else if (j == 2) {
       Energy = delta * calcEnergyR2(Population[i].FeedEff);
       R2 -= Energy;
    }
    Population[i].Fitness += Energy;
    std::cout << "Individual " << i << " now has a fitness of " << Population[i].Fitness << "\n";
    std::cout << "Resource 1: " << R1 << ", Resource 2: " << R2 << "\n";
}

std::vector<double> getFitness(std::vector<Individual> Population) {
    std::vector<double> Fitness(n);
    int NrR1 = 0;
    int NrR2 = 0;
    double AvgR1 = 0.0;
    double AvgR2 = 0.0;
    for (int i = 0; i < Population.size(); ++i) {
        Fitness[i] = Population[i].Fitness;
        if (Population[i].FeedEff < 0) {
            ++NrR1;
            AvgR1 += Population[i].FeedEff;
        }
        else {
            ++NrR2;
            AvgR2 += Population[i].FeedEff;
        }
    }
    ofs << NrR1 << "," << AvgR1 / NrR1 << "," << NrR2 << "," << AvgR2 / NrR2 << "\n";
    return Fitness;
}

void reproduce(std::vector<Individual> &Population, std::vector<double> &Fitness) {
    std::vector<Individual> NewPopulation = Population;
    std::discrete_distribution<int> chooseParent(Fitness.begin(), Fitness.end());
    for (int i = 0; i < NewPopulation.size(); ++i) {
        int p = chooseParent(rng);
        NewPopulation[i] = Population[p];
        NewPopulation[i].Fitness = 0.0;
        NewPopulation[i].Prob = 1.0;
    }
    Population = NewPopulation;
}

void mutate(std::vector<Individual> &Population) {
    std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
    for (Individual i : Population) {
        double r = chooseFraction(rng);
        if (r < mu) {
            std::normal_distribution<double> chooseMutation(i.FeedEff, sigma);
            i.FeedEff = chooseMutation(rng);
            if (i.FeedEff < -1.0) {
                i.FeedEff = -1.0;
            }
            else if (i.FeedEff > 1.0) {
                i.FeedEff = 1.0;
            }
        }
    }
}

void simulate(std::vector<Individual> &Population) {
    for (int g = 0; g < t; ++g) {
        std::cout << "Generation " << g << " started\n";
        R1 = MaxR1;
        R2 = MaxR2;
        bool enoughFood = true;
        while (enoughFood) {
            int i = chooseIndividual(Population);
            int j = chooseResource();
            if (j != 0) {
                if (doesIndAccept(Population[i], j)) {
                    std::cout << "Individual " << i << " chose to eat resource " << j << "\n";
                    eat(Population, i, j);
                    Population[i].Prob /= 2.0;
                }
                else {
                    std::cout << "Individual " << i << " chose to reject resource " << j << "\n";
                }
            }
            else {
                enoughFood = false;
                std::cout << "No more food\n";
            }
        }
        std::vector<double> Fitness = getFitness(Population);
        reproduce(Population, Fitness);
        //mutate(Population);
        std::cout << "Generation " << g << " completed\n";
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