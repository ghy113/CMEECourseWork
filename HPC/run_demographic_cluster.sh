#!/bin/bash

#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=1:mem=1gb

module load anaconda3/personal

echo "R about the stochastic model is about to run."

R --vanilla < $HOME/ghy113/hg2423_HPC_2023_demographic_cluster.R
mv stochastic_* $HOME/ghy113

echo "R (stochastic model) has finished running."
