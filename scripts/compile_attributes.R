#! /usr/bin/env Rscript
library (Rcpp) # Load Rcpp library
setwd("~/cartogramone") # Move into package wd
compileAttributes(verbose = TRUE) # to recompile the package 