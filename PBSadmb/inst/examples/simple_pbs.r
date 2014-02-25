# Sample R code for "PBSadmb" with the example "simple_pbs.tpl"

# Before sourcing this file, have a valid file "ADopts.txt" in the 
# current working directory or a 2-column text file of paths
# (e.g. "ADpaths.txt", but user can specify any name).

# Load R packages; ignore the GUI for now.
require(PBSmodelling); require(PBSadmb)
readADopts("Adopts.txt"); readADpaths("ADpaths.txt")

# Make and run "simple_pbs.exe"
makeAD("simple_pbs"); runAD("simple_pbs");

# Read and unpack the report;
# i.e., create R variables with the same names used in "simple_pbs.tpl"
simplePBS <- readList("simple_pbs.rep"); unpackList(simplePBS);

# Plot the data
plot(x,y); lines(x,ypred,col="red",lwd=2);

# Check the calculations in R

ypredR <- a + b*x; nobs <- length(x);
fvalR <- nobs*log(sigma) + sum((ypredR-y)^2)/(2.0*sigma^2)

cat("Functions values (ADMB & R):\n");
cat(fval,"   ",fvalR,"\n")

cat("Predictions (ADMB & R):\n");
cat(ypred,"\n");
cat(ypredR,"\n");

