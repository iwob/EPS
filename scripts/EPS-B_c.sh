#!/bin/bash

# Variant options:
# --fillHoles false     ('Baldwinian' variant, EPS-B)
# --eps.useConstantProvider false    (NO randomly generated constants as terminals)
# --eps.useInputVarsAsTerminals true     (original input variables are added as terminals)

variant="--eps.fillHoles false --eps.useConstantProvider false --eps.useInputVarsAsTerminals true"
problem="--eps.pathTests data/int/int1.csv --eps.holesConsts constInt:Int"


scala -cp "eps_2.11-1.0.jar:fuel_2.11-1.0.jar:swim_2.11-1.0.jar" eps.app.Main --seed 0 --populationSize 25 --maxGenerations 10 --operatorProbs 0.5,0.5 --tournamentSize 7 --initMaxTreeDepth 4 --maxSubtreeDepth 4 --parEval false --deleteOutputFile true --eps.logic NIA --eps.silent false --eps.saveLogs false --eps.pathToPySV pysv/main.py $problem $variant
