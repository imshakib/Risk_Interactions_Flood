# Risk Interactions
Coding hypothetical graphs to show how accounting for climate change and uncertainty in flood hazard, and levee effect in flood exposure would impact flood risk pdfs and expected values.
Script creates conceptual graphs to show interactions among flood hazard, exposure, and risk.
The output of this code is stored on Figshare at:
https://figshare.com/articles/figure/Risk_Interactions_2/14175395

The code doesn't require input data to run. It generates random inputs for the plots.
R Packages used in this code are as follows: 
1- "VGAM" used to create random data on a skewed normal distribution curve, 
2- "ggplot2" used to create the plots, 
3- "ggpubr" used to arrange different plots, and 
4- "ggExtra" used to create the diagnostic plot of marginal densities.
To install packages in R use the command: 
install.packages("package name") 

For questions, comments, feedback, or any concerns, please email Iman Hosseini-Shakib at ishakib@gmail.com

 Authors: Iman Hosseini-Shakib (ishakib@gmail.com)
          Klaus Keller (kzk10@psu.edu)
 Recoded by: Vivek Srikrishnan (vs498@cornell.edu)
 Code verification: Matthew Lisk (mdl5548@psu.edu)
       
 Copyright 2021 Iman Hosseini-Shakib and Klaus Keller
 This file is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This file is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this file.  If not, see <http://www.gnu.org/licenses/>.
 
 
