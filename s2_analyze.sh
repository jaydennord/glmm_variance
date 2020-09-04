#!/bin/bash
#SBATCH --array=0-749
#SBATCH --time=3-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=16384
#SBATCH --job-name=ana
#SBATCH --error=./slurm/job_%A_%a.err
#SBATCH --output=./slurm/job_%A_%a.out

module load R
Rscript 2-analyze.R $SLURM_ARRAY_TASK_ID

module swap R sas
sas 2-analyze.sas -sysparm "$SLURM_ARRAY_TASK_ID"
