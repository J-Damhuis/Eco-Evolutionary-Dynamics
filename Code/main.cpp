#include <iostream>
#include <vector>
#include <random>
#include <fstream>
#include <algorithm>

const int n = 1000; //Population size
const int g = 3000; //Number of generations
const int d = 10; //Number of feeding rounds per generation
const double mu = 0.5; //Mutation rate
const double sigma = 0.01; //Standard deviation of Gaussian distribution mutation sizes are taken from
double beta = 0.4; //Degree of optimal choice
double s = 1.0; //Selection coefficient
double delta = 0.2; //Slope of Michaelis-Menten dynamics resource acquisition
int seed = 1; //Seed
std::vector<double> MaxR = {10.0, 10.0}; //Maximum amount of resources
std::vector<double> R = MaxR; //Starting size of resources
std::vector<std::vector<int> > Ind = {{}, {}}; //Indices of individuals present at the resources
std::ofstream ofs("test.csv"); //Output of most interesting stuff
std::ofstream ofs2("heatmap.csv"); //Output of individuals' trait values
std::mt19937_64 rng;

class Individual {
public:
    double FeedEff; //Trait value
    double Food; //Food it has acquired during its lifetime
};

std::vector<Individual> createPopulation() {
    std::vector<Individual> Population(n);
    std::uniform_real_distribution<double> chooseValue(-1.0, -0.8);
    for (int i = 0; i < Population.size(); ++i) {
        Population[i].FeedEff = chooseValue(rng); //Give every individual a random trait value between -0.8 and -1.0
        Population[i].Food = 0.0; //Set food to 0
        //std::cout << i << ": " << Population[i].FeedEff << "\n";
    }
    //std::cout << "\n";
    return Population;
};

void shuffle(std::vector<Individual> &Population) { //Randomise the way the individuals are ordered
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

double calcEnergy(double x, int i) { //Get the energy a certain trait value can get at a certain resource
    return exp(-s * pow(x + pow(-1, i), 2));
}

void getFood(std::vector<Individual> &Population) { //Distribute food among all individuals
    for (int j = 0; j < Ind.size(); ++j) {
        double Sum = 0.0;
        for (int i = 0; i < Ind[j].size(); ++i) {
            Sum += calcEnergy(Population[Ind[j][i]].FeedEff, j); //Calculate sum of feeding efficiencies at each resource
        }
        for (int i = 0; i < Ind[j].size(); ++i) {
            Population[Ind[j][i]].Food += calcEnergy(Population[Ind[j][i]].FeedEff, j) * R[j] / (Sum + pow(delta, -1) - 1);
            //std::cout << Ind[j][i] << ": " << Population[Ind[j][i]].Food << "\n";
        }
    }
    //std::cout << "\n";
}

//OPTIMISE THIS FUNCTION
void chooseResource(std::vector<Individual> &Population) { //Make individuals distribute themselves over the resources
    std::vector<double> Sum = {0.0, 0.0};
    std::vector<double> Indiv = {0, 0};
    std::vector<double> FoundFood = {0.0, 0.0};
    for (int e = 0; e < d; ++e) {
        std::vector<double> TempSum = {0.0, 0.0};
        shuffle(Population);
        /*for (int i = 0; i < Population.size(); ++i) {
            std::cout << i << ": " << Population[i].FeedEff << "\n";
        }
        std::cout << "\n";*/
        R = MaxR;
        for (int i = 0; i < Population.size(); ++i) {
            std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
            double r = chooseFraction(rng);
            if (r > beta) {
                double r2 = chooseFraction(rng);
                int j = r2 > 0.5 ? 0 : 1;
                Ind[j].push_back(i);
                Sum[j] += Population[i].FeedEff;
                TempSum[j] += calcEnergy(Population[i].FeedEff, j);
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
                    Energy[j] = R[j] * calcEnergy(Population[i].FeedEff, j) / (Energy[j] + pow(delta, -1) - 1);
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
                TempSum[j] += calcEnergy(Population[i].FeedEff, j);
                Indiv[j] += 1.0;
                //std::cout << i << ": " << j << " - " << Energy[0] << " vs " << Energy[1] << "\n";
            }
        }
        //std::cout << "Round " << e << ":\n";
        for (int j = 0; j < FoundFood.size(); ++j) {
            FoundFood[j] += TempSum[j] / (TempSum[j] + pow(delta, -1) - 1);
            //std::cout << "-Resource " << j << ": " << TempSum[j] / (TempSum[j] + pow(delta, -1) - 1) << "(" << TempSum[j] << ")\n";
        }
        //std::cout << "\n";
        getFood(Population);
        for (int i = 0; i < Ind.size(); ++i) {
            Ind[i].clear();
        }
    }
    ofs << Indiv[0]/(Population.size()*d) << "," << Sum[0]/Indiv[0] << "," << FoundFood[0]/d << ","
        << Indiv[1]/(Population.size()*d) << "," << Sum[1]/Indiv[1] << "," << FoundFood[1]/d << "\n";
}

std::vector<double> getFitness(std::vector<Individual> &Population) { //Make a vector of the amounts of food the individuals have found
    std::vector<double> Fitness(Population.size());
    for (int i = 0; i < Fitness.size(); ++i) {
        Fitness[i] = Population[i].Food;
        //std::cout << i << ": " << Population[i].Food << "\n";
    }
    //std::cout << "\n";
    return Fitness;
}

void reproduce(std::vector<Individual> &Population, std::vector<double> &Fitness) { //Make population reproduce
    std::vector<Individual> NewPopulation = Population; //Clone population
    std::discrete_distribution<int> chooseParent(Fitness.begin(), Fitness.end());
    for (int i = 0; i < NewPopulation.size(); ++i) {
        int p = chooseParent(rng); //Choose random individual based on fitness
        Population[i] = NewPopulation[p]; //Add chosen individual to new population
    }
}

void mutate(std::vector<Individual> &Population) { //Allow mutations to occur within population
    std::uniform_real_distribution<double> chooseFraction(0.0, 1.0);
    for (int i = 0; i < Population.size(); ++i) {
        double r = chooseFraction(rng); //Choose random number between 0 and 1
        if (r < mu) { //If a mutation occurs
            std::normal_distribution<double> chooseMutation(Population[i].FeedEff, sigma);
            Population[i].FeedEff = chooseMutation(rng); //Choose size of the change from Gaussian distribution
            if (Population[i].FeedEff < -1.0) {
                Population[i].FeedEff = -1.0; //If lower bound was passed, set value back to lower bound
            }
            else if (Population[i].FeedEff > 1.0) {
                Population[i].FeedEff = 1.0; //If upper bound was passed, set value back to upper bound
            }
        }
    }
}

bool sortPop(Individual i, Individual j) { //Help function for sorting the population based on trait values
    return (i.FeedEff > j.FeedEff);
}

void simulate(std::vector<Individual> &Population) { //Run simulation
    ofs2 << "0";
    sort(Population.begin(), Population.end(), sortPop); //Sort initial population based on trait values
    for (int i = 0; i < Population.size(); ++i) {
        ofs2 << "," << Population[i].FeedEff;
    }
    ofs2 << "\n";
    for (int t = 0; t < g; ++t) { //For every generation
        ofs << t << ",";
        chooseResource(Population);
        std::vector<double> Fitness = getFitness(Population);
        reproduce(Population, Fitness);
        mutate(Population);
        ofs2 << t+1;
        sort(Population.begin(), Population.end(), sortPop); //Sort population again based on trait values
        for (int i = 0; i < Population.size(); ++i) {
            ofs2 << "," << Population[i].FeedEff;
        }
        ofs2 << "\n";
        std::cout << "Finished generation " << t << "\n";
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
    for (int i = 0; i < Population.size(); ++i) {
        ofs2 << ",Individual " << i;
    }
    ofs2 << "\n";
    simulate(Population);
    return 0;
}