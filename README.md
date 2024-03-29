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
 
 ## Possible Issues:
 
 This code has been tested on Windows and Mac local machines as well as the Penn State HPC system including ROAR and Firkin, as well as an Ubuntu Singularity. 
 - A reported issue was that the installation of the "ggpubr" package produced some errors on the Linux machines due to a configuration issue with finding 'libcurl'. The "ggarrange" command is used from the "ggpubr"in the code.
 - It is recommended that you run the code on high-performance computation facilities. You might encounter a resource issue while running the code on local machines or while trying to open the plot in the pdf format. The pdf plot might be unable to be viewed in its entirety on some Mac and Win10 machines with any of the pdf viewers used as a matter of resource issues. When the files are clicked on, the viewer window will open. After a few minutes parts of plots might appear, but then the viewers might stop responding. You might be able to find a work around on these machines, by importing the pdf into GIMP (https://www.gimp.org/), but even that import process might take around 5-10 minutes. 
 - In case of resource issues when running the code on local machines, you might want to consider using a smaller sample size which is currently 1e6 (nsamp on line 39). Running with an order of magnitude fewer samples (nsamp=1e5) will produce the curves that are not "smooth enough" but the main point of the plot to show the impact of uncertainties on the risk will be there.
  
 ## Copyright 
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
 
 
