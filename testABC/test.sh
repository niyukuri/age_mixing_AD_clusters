#!/bin/bash -l
#
#PBS -N testABC
#PBS -P CBBI1106
#PBS -q smp
#PBS -l select=1:ncpus=24:mpiprocs=24:nodetype=haswell_reg
#PBS -l walltime=04:00:00
#PBS -m be
#PBS -M niyukuri@aims.ac.za



ulimit -s unlimited


module add chpc/R/3.4.4-gcc7.2.0


cd /mnt/lustre/users/dniyukuri/testMABC

Rscript testscript.R
