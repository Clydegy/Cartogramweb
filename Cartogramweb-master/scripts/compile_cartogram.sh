#! /bin/bash


# Permission to execute R script
chmod +x ~/cartogramone/scripts/cartogram_package_Rscript.R

# Change directory to execture R script
cd ~/cartogramone/scripts


# Execute R scipt
./cartogram_package_Rscript.R

# This should modify the cartogramone package
# We now build and install the R package

R CMD build ~/cartogramone
mv cartogramone_1.0.tar.gz ~/
R CMD INSTALL ~/cartogramone_1.0.tar.gz
