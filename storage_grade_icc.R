library(ggplot2)
library(irr)
library(readxl)

setwd("") 
storage = read_excel("Fluid\ Preservation\ Outcomes\ -\ Public.xlsx", sheet = "Aldehyde")

storage$grade1 = storage$`Effect on morphology grade (Reviewer 1; 0 = no or minimal storage artifact, 1 = partial storage artifact, 2 = near-total/total storage artifact)`
storage$grade2 = as.numeric(storage$`Effect on morphology grade (Reviewer 2)`)

storage_grades_icc = cbind(as.numeric(storage$grade2), as.numeric(storage$grade1))
storage_icc_nona = storage_grades_icc[!is.na(storage_grades_icc[,1]), ] 

icc(storage_icc_nona, model = "twoway", type = "agreement", unit = "single")
