#! /usr/bin/env Rscript
input_vect <- c("randomprogram" ,"/home/aquinas/Cartogram_Research/Cartogram_Files/US_low48_more_nds.gen" , "/home/aquinas/Cartogram_Research/Cartogram_Files/low48_pop.dat")

library (cartogramone) # Load Rcpp library

output_df <- transformMap (input_vect)

# Code to process output_df

