#include <iostream>
#include <vector>
#include <random>
#include <fstream>
#include <algorithm>
#include <iomanip>

#include <array>

#include <chrono>

const int n = 1000; //Number of individuals
const int g = 50000; //Number of generations
const int h = 2; //Number of habitats
const int r = 2; //Number of resources
const int d = 10; //Number of feeding rounds per generation
const double mu = 0.001; //Mutation rate
const double sigma = 0.02; //Standard deviation of Gaussian distribution the size of a mutation is taken from
const double m = 0.1; //Migration rate
double beta = 0.5; //Degree of optimal choice
double s = 1; //Selection coefficient
double delta = 0.2; //Slope of Michaelis-Menten function for resource acquisition
double q = 0.0; //Habitat asymmetry
int seed = 1;
const int save = 1; //Save feeding efficiencies to file every so many generations
const std::array<double, 2> R = {10.0, 10.0}; //Total size of resources
std::ofstream seedoutput("seed.txt"); //Output file with seed
std::ofstream ofs("data.csv"); //Output file with habitat dynamics over time
std::ofstream ofs2("heatmap.csv"); //Output file with individuals' trait values over time
std::ofstream ofs3("habitat.csv"); //Output file with individuals' habitats over time




// random class
struct rnd_j {
public:
    std::mt19937_64 rng; // needs to be public to use in std::shuffle

    void set_seed(size_t seed) {
        rng = std::mt19937_64(seed);
    }


    bool bernoulli(double p) {
        std::bernoulli_distribution d(p);
        return(d(rng));
    }

    double pickFeedEff() {
        return chooseValue(rng);
    }

    double pickFraction() {
        return chooseFraction(rng);
    }

    int pickResource() {
        return chooseResource(rng);
    }

    double drawMutation() {
        return add_mutation(rng);
    }

    double uniform() {
        return chooseFraction(rng);
    }


private:
    std::normal_distribution<double> add_mutation = std::normal_distribution<double>(0.0, sigma);
    std::uniform_int_distribution<int> chooseResource = std::uniform_int_distribution<int>(0, 1);
    std::uniform_real_distribution<double> chooseValue = std::uniform_real_distribution<double>(-1.0, -0.8);
    std::uniform_real_distribution<double> chooseFraction = std::uniform_real_distribution<double>(0.0, 1.0);
} rndgen;


struct Individual {

    Individual() {
        Food = 0.0;
        Habitat = 0;
        FeedEff = rndgen.pickFeedEff();
    }


    double FeedEff; //Ecological trait value
    double Food; //Found food (fitness)
    int Habitat; //Habitat individual is currently in
};

struct Resource {

    Resource() {
        Size = 0.0;
        Sum = 0.0;
        Indiv = 0.0;
        FoundFood = 0.0;
    }

    Resource(int habitat, int resource) {
        Size = habitat == resource ? R[resource] * (1.0 / h) * (1.0 + q) : R[resource] * (1.0 / h) * (1.0 - q);
        Sum = 0.0;
        Indiv = 0.0;
        FoundFood = 0.0;
    }

    double Size = 0.0; //Total size of the resource within a habitat
    double Sum = 0.0; //Sum of trait values of individuals feeding on this resource (only used to keep track of mean trait value within resource)
    double Indiv = 0.0; //Total number of individuals feeding on this resource (only used to keep track of mean number of individuals on each resource)
    double FoundFood = 0.0; //Total amount of this resource found during a feeding season
};

struct Habitat {

    Habitat() {
        Resources = {Resource(), Resource()};
        SumFeedEff = {0.0, 0.0};
        Ind = {std::vector<int>(), std::vector<int>()};
    }

    Habitat(int habitat) {
        for (int resource = 0; resource < r; ++resource) {
            Resources[resource] = Resource(habitat, resource);
        }
        SumFeedEff = {0.0, 0.0};
        Ind = {std::vector<int>(), std::vector<int>()};
    }

    std::array<Resource, r> Resources; //Resources within a habitat
    std::array<double, r> SumFeedEff; //Sum of feeding efficiencies of individuals feeding on resources within a habitat (used for decision making)
    std::array< std::vector<int>, r> Ind; //Lists of individuals feeding on resources within a habitat
};

std::vector<Individual> createPopulation() {
    std::vector<Individual> Population(n);
    return Population;
}

std::vector<Habitat> createHabitats() {
    std::vector<Habitat> Habitats;
    for (int i = 0; i < h; ++i) {
        Habitats.push_back(Habitat(i));
    }
    return Habitats;
}

void shuffle(std::vector<Individual> &Population) { //Randomise order of individuals
    std::shuffle(Population.begin(), Population.end(), rndgen.rng);
}

double calcEnergy(const double local_s, const double x, const int i) {
    auto dd = x * x + 1;
    i == 0 ? dd += 2 * x : dd -= 2 * x;
    return exp(-local_s * dd);
}

int makeChoice(const Individual& focal_ind, const std::vector<Habitat> &Habitats) {
    int l; //Variable which will output the decision that's made
    if (beta == 0) { //If individual chooses randomly (needed to avoid dividing by 0)
        l = rndgen.pickResource(); //Set variable to either 0 or 1
    }
    else { //If individual has some degree of optimal choice
        std::array<double, r> Fitness = Habitats[focal_ind.Habitat].SumFeedEff;
        for (size_t resource = 0; resource < r; ++resource) {
            auto energy_calced = calcEnergy(s, focal_ind.FeedEff, resource);

            Fitness[resource] =
                    Habitats[focal_ind.Habitat].Resources[resource].Size *
                    energy_calced /
                    (energy_calced +
                     Fitness[resource] +
                     1.0 / delta - 1); //Calculate how much food individual would get if it fed on the resources
        }

        double a = std::abs(Fitness[0] - Fitness[1]); //Calculate absolute difference between expected fitnesses
        double b = a + sqrt( (1 - beta) * a * a) / beta;

        auto min_noise = - b / 2;
        auto max_noise = b / 2;
        auto range_noise = max_noise - min_noise;


        for (int resource = 0; resource < r; ++resource) {
            Fitness[resource] += min_noise + rndgen.uniform() * range_noise;   //getAssessment(rndgen.rng); //Get noisy assessment
        }
        if (Fitness[0] > Fitness[1]) { //If first resource is perceived to be more advantageous
            l = 0; //Choose first resource
        } else if (Fitness[0] == Fitness[1]) { //If resources are perceived to be equally good
            l = focal_ind.FeedEff < 0.0 ? 0 : 1; //Choose resource the individual is better adapted to
        } else { //If second resource is perceived to be more advantageous
            l = 1; //Choose second resource
        }
    }
    return l; //Output the made decision
}

void update_and_get_food(std::vector<Individual>& Population, Habitat& Habitat_) {
    for (int resource = 0; resource < r; ++resource) {
        int vec_size = 1;
        if (!Habitat_.Ind[resource].empty()) vec_size += Habitat_.Ind[resource].back();

        std::vector<double> energies(vec_size, 0.0);

        for (const auto& i : Habitat_.Ind[resource]) {
            Habitat_.Resources[resource].Sum += Population[ i ].FeedEff;
            energies[i] = calcEnergy(s, Population[ i ].FeedEff, resource);
        }

        auto TempSum = std::accumulate(energies.begin(), energies.end(), 0.0);

        Habitat_.Resources[resource].FoundFood += TempSum / (TempSum + 1.0 / delta - 1); //Calculate total amount of resource found during feeding round
        Habitat_.Resources[resource].Indiv += Habitat_.Ind[resource].size(); //Update total number of individuals feeding on resource

        for (const auto& index : Habitat_.Ind[resource]) {
            Population[index].Food +=
                    //calcEnergy(s, Population[index].FeedEff, resource) *
                    energies[index] *
                    Habitat_.Resources[resource].Size / (TempSum + 1.0 / delta - 1); //Calculate found food for each individual
        }
    }
}




void chooseResource(std::vector<Individual> &Population) {
    std::vector<Habitat> Habitats = createHabitats(); //Reset habitats
    for (int round = 0; round < d; ++round) {
        shuffle(Population); //Randomise the order of the individuals
        for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
            int j = makeChoice(Population[individual], Habitats); //Let individual make choice
            Habitats[Population[individual].Habitat].Ind[j].push_back(individual); //Add individual to list of individuals feeding on the chosen resource
            Habitats[Population[individual].Habitat].SumFeedEff[j] += calcEnergy(s, Population[individual].FeedEff, j); //Add individual's feeding efficiency to total feeding efficiencies at resource
        }
        for (int habitat = 0; habitat < static_cast<int>(Habitats.size()); ++habitat) {
            update_and_get_food(Population, Habitats[habitat]);
            for (int resource = 0; resource < r; ++resource) {
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
    std::vector<double> Fitness(Population.size()); //Create list of fitnesses
    for (size_t individual = 0; individual < Population.size(); ++individual) {
        Fitness[individual] = Population[individual].Food; //Add each individual's found food to fitness list
    }
    return Fitness;
}

void reproduce(std::vector<Individual> &Population, std::vector<double> &Fitness) {
    std::vector<Individual> NewPopulation; //Create new population
    std::discrete_distribution<int> chooseParent(Fitness.begin(), Fitness.end());
    for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
        int p = chooseParent(rndgen.rng); //Choose which individual reproduces based on fitness values
        NewPopulation.push_back(Population[p]); //Place copy of reproducing individual in new population
        NewPopulation.back().Food = 0.0; //Set found food of new individual to 0
    }
    std::swap(Population, NewPopulation); //Replace old population with new population
}

void mutate(std::vector<Individual> &Population) {

    for (auto& i : Population) {
        if (rndgen.bernoulli(mu)) { //if mutation takes place
            i.FeedEff += rndgen.drawMutation(); //Set new trait value to a small deviation from the old trait value taken from a normal distribution
            if (i.FeedEff < -1.0) {
                i.FeedEff = -1.0; //Make sure trait value is at least -1
            }
            else if (i.FeedEff > 1.0) {
                i.FeedEff = 1.0; //Make sure trait value is at most 1
            }
        }
    }
}

void migrate(std::vector<Individual>& Population) {

    std::vector<int> all_migrants;
    std::binomial_distribution<int> chooseMigrations(Population.size(), m);
    int num_migrants = chooseMigrations(rndgen.rng);

    for (int i = 0; i < num_migrants; ++i) {
        std::uniform_int_distribution<int> rand_index(i, Population.size() - 1);
        int index = rand_index(rndgen.rng);
        std::swap(Population[i], Population[index]);
        Population[i].Habitat = 1 - Population[i].Habitat;
    }
}


bool sortPop(Individual i, Individual j) {
    return (i.FeedEff > j.FeedEff); //Helps sort the individuals by trait value to make the analysis easier
}

void simulate(std::vector<Individual> &Population) {
    ofs2 << "0";
    ofs3 << "0";
    sort(Population.begin(), Population.end(), sortPop);
    for (const auto& i : Population) {
        ofs2 << "," << i.FeedEff;
        ofs3 << "," << i.Habitat;
    }
    ofs2 << "\n";
    ofs3 << "\n";
    auto clock_start = std::chrono::system_clock::now();
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
            //for (int individual = 0; individual < static_cast<int>(Population.size()); ++individual) {
            for (const auto& i : Population) {
                ofs2 << "," << i.FeedEff;
                ofs3 << "," << i.Habitat;
            }
            ofs2 << "\n";
            ofs3 << "\n";

            auto clock_now = std::chrono::system_clock::now();
            std::chrono::duration<double> elapsed_seconds = clock_now - clock_start;
            auto millis = std::chrono::duration_cast<std::chrono::milliseconds>(elapsed_seconds).count();

            std::cout << "Finished generation " << t+1 << " " << millis << "ms\n";
            clock_start = std::chrono::system_clock::now();
        }
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
    rndgen.set_seed(seed);
    std::cout << "β = " << beta << " | s = " << s << " | δ = " << delta << " | q = " << q << "\n";
    std::cout << "Using seed " << seed << "\n";
    std::vector<Individual> Population = createPopulation();
    if (!ofs.is_open()) {
        return 1;
    }
    if (!ofs2.is_open()) {
        return 1;
    }
    seedoutput << seed;
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
