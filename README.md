## PBSadmb: ADMB for R using scripts or GUI ##
&copy; Fisheries and Oceans Canada (2008-2020)

The R package **PBSadmb** gives complete R support to the external program AD Model Builder (<a href="http://www.admb-project.org/">ADMB</a>) released into the public domain in 2009. The program offers users a remarkably efficient tool for estimating parameters and their uncertainty, based on complex nonlinear statistical models.

In a standard ADMB installation, users would interact with the program via a DOS command shell (in Windows) or a bash shell (in Linux or Mac OS X). The package **PBSadmb** makes it possible to interact entirely from an R console, as a common interface for all operating systems. A single R script can encapsulate commands to ADMB, as well as all analyses that follow. The package includes protocols for writing code to make the integration between R and ADMB almost seamless.

**PBSadmb** also provides a Graphical User Interface (GUI) that facilitates the steps required for a complete ADMB analysis. Both new and experienced users can use the GUI for tutorial and educational purposes.

You can obtain **PBSadmb** from the Comprehensive R Archive Network (<a href="https://cran.r-project.org/web/packages/PBSadmb/index.html">CRAN</a>). Always use the current version of **PBSadmb** with the most recent version of **PBSmodelling**, another package available from CRAN. 

**PBSadmb** represents just one of a series of R packages developed at the Pacific Biological Station (<a href="http://www.pac.dfo-mpo.gc.ca/science/facilities-installations/index-eng.html#pbs">PBS</a>) in Nanaimo, British Columbia. A more advanced version of **PBSadmb** might be available at <a href="https://github.com/pbs-software">pbs-software on GitHub</a>. Any evolving package (Windows binary and source tarball) is built after using CRAN's rigorous `R CMD check --as-cran` routine (on a Windows 7 64-bit system) and posted to <a href="https://drive.google.com/drive/folders/0B2Bkic2Qu5LGOGx1WkRySVYxNFU?usp=sharing">Google Drive</a>. Most of the time, the revision on <a href="https://github.com/pbs-software/pbs-admb">GitHub</a> can be built (supposedly) in R using `devtools::install_github("pbs-software/pbs-admb")`; however, not every revision has been checked for CRAN worthiness.

As with any freely available product, there is no warranty or promise that **PBSadmb** will perform adequately for all circumstances. Additionally, coding errors are possible, and users should contact the package maintainer if bugs are detected.

Maintainer: <a href="mailto:rowan.haigh@dfo-mpo.gc.ca">Rowan Haigh</a>

<p align="right"><img src="DFOlogo_small.jpg" alt="DFO logo" style="height:30px;"></p> 

