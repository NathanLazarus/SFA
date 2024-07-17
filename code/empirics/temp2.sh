#!/bin/bash
#SBATCH -n 12 #Request 4 tasks (cores)
#SBATCH -N 1 #Request 1 node
#SBATCH -t 2-23:59 #Request runtime of 30 minutes                                                                                               
#SBATCH -p sched_mit_econ  #Run on sched_engaging_default partition
#SBATCH --mem-per-cpu=64000 #Request 4G of memory per CPU
#SBATCH -o slurm_output/output_%j.txt #redirect output to output_JOBID.txt
#SBATCH -e slurm_output/error_%j.txt #redirect errors to error_JOBID.txt
#SBATCH --mail-type=BEGIN,END #Mail when job starts and ends
#SBATCH --mail-user=ian_s@mit.edu #email recipient


module load R/4.1.0


Rscript code/testBigFile.R