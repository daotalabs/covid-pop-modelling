#!/bin/sh

#SBATCH -J simulation
#SBATCH --array=1-625
#SBATCH --time=3-00:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --mail-user=vietdao@uvic.ca
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --output=slurm-%A_%a.out
#SBATCH --account=def-lcowen

echo "The current directory is `pwd`"
echo "Start at `date`"

module load r/4.2.2
export R_LIBS=/home/vdao/R/

Rscript ./run_sim.R $SLURM_ARRAY_TASK_ID 1 >& ./log/log_${SLURM_JOB_ID}_${SLURM_ARRAY_TASK_ID}.txt

echo "Done at `date`"