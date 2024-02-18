#!/bin/bash

#SBATCH --job-name DGP2
#SBATCH --partition airs-cpu
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1


echo "Job Name:"  $SLURM_JOB_NAME
echo "Dir:" $SLURM_SUBMIT_DIR
echo "CPUs:" $SLURM_CPUS_PER_TASK
echo "process will start at : "
date
echo "++++++++++++++++++++++++++++++++++++++++"

module purge
module load R/conda-R4.3
export OPENBLAS_NUM_THREADS=1

Rscript --vanilla '2 servicepeople.R'
Rscript --vanilla '3 regression.R'











