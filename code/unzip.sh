#!/bin/bash
year=$1
sbatch <<EOT
#!/bin/bash
#SBATCH -t 0-23:59 #Request runtime of 1 day
#SBATCH -p sched_mit_econ  #Run on sched_engaging_default partition
#SBATCH --mem-per-cpu=64000 #Request 4G of memory per CPU
#SBATCH -o slurm_output/unzip/output_%j.txt #redirect output to output_JOBID.txt
#SBATCH -e slurm_output/unzip/error_%j.txt #redirect errors to error_JOBID.txt
#SBATCH --mail-type=BEGIN,END #Mail when job starts and ends
#SBATCH --mail-user=ian_s@mit.edu #email recipient

tar xzvf "data/SCANNER_DATA_"$year".tgz" "data/nielsen_extracts/RMS/$year"
EOT