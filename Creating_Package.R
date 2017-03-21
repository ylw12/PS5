library(devtools)
library(roxygen2)

# Create the basic structure of the package.
create("ModelfitnessPack")

setwd("/Users/luweiying/Desktop/ASP/PS5/PS5_Luwei")

# Now, go to the FitnessEvaluation.R under the R folder to write the functions, including
# the codes for documentations at the very beginning.

# "Call" the the package with the functions I wrote.
current.code <- as.package("ModelfitnessPack")

# Write NAMESPACE and FitnessEvaluation.Rd (in MAN folder)
document(current.code)

# Edit the DESCRIPTION FILE
Des <- read.dcf("ModelfitnessPack/DESCRIPTION")
Des[1, 2] <- "Calculate the fit statistics of models" 
Des[1, 3] <- "0.0.1"
Des[1, 4] <- "person(\"Luwei\", \"YING\", email = \"luwei.ying@wustl.edu\", role = c(\"aut\", \"cre\"))"
Des[1, 5] <- "Calculate the fit statistics of models, given the predicted values and observaed values."
Des[1, 7] <- "GPL (>= 2)"

# Save the new version of the  DESCRIPTION FILE
write.dcf(Des, "ModelfitnessPack/DESCRIPTION")

# Package and document the code again.
current.code <- as.package("ModelfitnessPack")
document(current.code)

# Check if the package content
check(current.code)

# Build a local copy to share
build(current.code, path=getwd())

# Install the package
install(pkg=current.code, local=TRUE)

