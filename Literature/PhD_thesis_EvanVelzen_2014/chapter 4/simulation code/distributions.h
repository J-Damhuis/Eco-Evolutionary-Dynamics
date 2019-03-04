//---------------------------------------------------------------------------
//draw random numbers from Poisson, gamma, negative binomial and bivariate negative binomial distributions
//---------------------------------------------------------------------------
#ifndef DISTRIBUTIONS_H
#define DISTRIBUTIONS_H

#include <vector>
#include <math.h>
#include <fstream>
#include <stdlib.h>
#include "random.h"
#pragma hdrstop

using namespace std;
//---------------------------------------------------------------------------
double check1(double p, double k)
{
	double x = pow(p, 1 / k);
	if (exp(-x) > Uniform())
		return x;
	return -1;
}
//---------------------------------------------------------------------------
double check2(double p, double k, double b)
{
	double x = -log((b - p) / k);
	if (pow(x, k - 1) > Uniform())
		return x;
	return -1;
}
//---------------------------------------------------------------------------
double gammapart(double k)
{
	double b = (exp(1) + k) / exp(1);
	double x;
	do{
		double p = Uniform() * b;
		if (p <= 1)
			x = check1(p, k);
		else x = check2(p, k, b);
	}
	while (x < 0);
	return x;
}
//---------------------------------------------------------------------------
double gammaint(int k)
{
	int i = 0;
	double p = 1;
	do
	{
		p *= Uniform();
		i++;
	}
	while (i < k);
	return -log(p);
}
//---------------------------------------------------------------------------
double gamma (double k, double scale)
{
	//draw number from gamma distribution
	double k_part, x1, x2;
	int k_int;
	k_int = int(k);
	k_part = k - k_int;
	if (k >= 1.0)
		x2 = double(gammaint(k_int));
	else x2 = 0;
	x1 = gammapart(k_part);
	return (x1 + x2) / scale;	
}
//---------------------------------------------------------------------------
int poisson(double average)
{
	//draw number from Poisson distribution
	double L = exp(-average);
	int k = 0;
	double p = 1.0;
	do{
		k++;
		p *= Uniform();
	}
	while (p > L);
	return k - 1;
}
//---------------------------------------------------------------------------
int negbinom2(double mean, double k, double g)
{
	//draw number from negative binomial distribution where the gamma part is given by "g"
	double p = 1.0 - mean / (mean + k);
	double lambda = g * ((1.0 - p) / p);
	int x = poisson(lambda);
	return x;
}
//---------------------------------------------------------------------------
int negbinom1(double p, double k)
{
	//draw number from negative binomial distribution
	double lambda = gamma(k, 1.0) * ((1.0 - p) / p);
	int x = poisson(lambda);
	return x;
}
//---------------------------------------------------------------------------
void distributenegbinom (double k, double corr, double mean1, double mean2, int &enc1, int &enc2)
{
	//draw two numbers from bivariate negative binomial distribution
	if (corr > 0.9999)
		corr = 0.9999;
	double theta = corr / (1.0 - corr);
	double pi = 1.0 / (1.0 + theta);
	double y = double(negbinom1(pi, k));
	double x1 = gamma(k + y, 1.0 + theta);
	double x2 = gamma(k + y, 1.0 + theta);
	enc1 = negbinom2(mean1, k, x1);
	enc2 = negbinom2(mean2, k, x2);
}
//---------------------------------------------------------------------------

#endif