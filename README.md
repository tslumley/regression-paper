# regression-paper
Code and data for paper on regression with survey data, for Statistical Science

Raw data files from NHANES 2003-4 and 2005-6
* bmx_c.xpt, bmx_d.xpt: body size data 
* bpx_c.xpt, bpx_d.xpt: blood pressure data
* demo_c.xpt, demo_d.xpt: basic demographic and weighting data
* dr1tot_c.xpt, dr1tot_d.xpt: dietary data

Raw data file from Ille-et-Vilaine (o)esophageal cancer study, via Prof. Norm Breslow
* tuynsc.txt

Code files:
* data-setup.R produces combined-data.csv with the combined data and combined-data.rda with survey object
* figure1.R produces figure 1
* figure2.R produces figure 2
* ish-regression.R fits all the models and produces all the output in Sections 3 and 4
* casecontrol-ille-et-vilaine.R fits the models and produces the output in Section 6.

