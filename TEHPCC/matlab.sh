#!/bin/sh
#$ -V
#$ -cwd
#$ -S /bin/bash
#$ -N NSGALin
#$ -o $JOB_NAME.o$JOB_ID
#$ -e $JOB_NAME.e$JOB_ID
#$ -q serial 
#$ -pe fill 1
#$ -P hrothgar
ifort  model_check.f control_check.f
./a.out