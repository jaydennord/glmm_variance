#!/bin/bash
#SBATCH --time=1-00:00:00
#SBATCH --mem-per-cpu=4096
#SBATCH --job-name=gen
#SBATCH --error=./slurm/job_%J.err
#SBATCH --output=./slurm/job_%J.out

module load R
Rscript 1-generate.R 250
