# Sample R code for "PBSadmb" with the example "vonb.tpl"

# Before sourcing this file, have a valid file "ADopts.txt" in the 
# current working directory.

# Load R packages; ignore the GUI for now.
require(PBSmodelling); require(PBSadmb);

# Make and run "vonb.exe"
makeAD("vonb"); runAD("vonb");

# Read and unpack the report;
# i.e., create R variables with the same names used in "vonb.tpl"
vonb <- readList("vonb.rep"); unpackList(vonb);

# Plot the data
plot(age,size); lines(age,spred,col="red",lwd=2);

# Check the calculations in R

spredR <- Linf*(1-exp(-K*(age-t0)));
nobs   <- length(age);
fvalR  <- nobs*log(sigma) + sum((spredR-size)^2)/(2.0*sigma^2)

cat("Functions values (ADMB & R):\n");
cat(fval,"   ",fvalR,"\n")

cat("Predictions (ADMB & R):\n");
cat(spred,"\n");
cat(spredR,"\n");

