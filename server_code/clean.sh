#!/bin/bash
year=$1
nchunks=$2
sbatch <<EOT
#!/bin/bash
#SBATCH -t 0-23:59 #Request runtime of 1 day
#SBATCH --array=1-$nchunks                             
#SBATCH -p sched_mit_econ  #Run on sched_engaging_default partition
#SBATCH --mem-per-cpu=16000 #Request 4G of memory per CPU
#SBATCH -o slurm_output/output$year/output_%j_%a.txt #redirect output to output_JOBID.txt
#SBATCH -e slurm_output/output$year/error_%j_%a.txt #redirect errors to error_JOBID.txt


module load R/4.1.0

Rscript SFA_github/server_code/cleanNielsenMovement.R $year \${SLURM_ARRAY_TASK_ID}
EOT