#!/bin/sh

#PBS -lwalltime=12:00:00
#PBS -lselect=1:ncpus=1:mem=1gb

module load anaconda3/personal

echo "R is about to run"

R --vanilla < $HOME/ghy113/hg2423_HPC_2023_neutral_cluster.R
mv neutral_cluster_* $HOME/ghy113

echo "R has finished running"
