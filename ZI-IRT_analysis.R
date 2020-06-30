# This script aims to analyze data from the the sidi scale to measure medically
# unexplained symptoms through a zero-inflated IRT model (see Wall, Park, & Moustaki, 2015).


# Prepare Enviroment ----
library(MplusAutomation)

# Read data and export data to Mplus ----

load("Data.analysis.RData")
sidi <- Data.analysis

# Exclude unnecessary variables.
sidi <- sidi[, c(1, 2, 28, grep("rec", names(sidi)))]
sidi$AgeCat <- as.numeric(sidi$AgeCat)

# Checking how many persons responded 0 to all items. 
items <- sidi[, -(1:3)]
itemsZI <- items[apply(items, 1, sum) != 0, ]
summary(apply(itemsZI, 2, mean))

# Export data to Mplus. 
prepareMplusData(as.data.frame(as.matrix(sidi)), "sidi.dat", inpfile = FALSE)

# Due to software availability, I fitted the model in peregrine: The high performance
# cluster computer of the univerisity of Groningen.

# I fitted the zero-inflated IRT model to the 23 items, using two and three classes
# as suggested by Wall(2015). Given the AIC and BIC, the model with two classes seems 
# to fit best the data. However, in both of the models not a single person was classified 
# in the class 1 (non-pathological class).

# Next, I read Mplus output into R, to look at the estimated items' parameters.
# The function available in MplusAutomation readModels() did not work. It might 
# not be updated to handle mixture models. Therefore, I simply read the output
# with readLines.

fit_23 <- readLines("sidi.out", skipNul = TRUE)
# Exclude empty lines.
fit_23 <- fit_23[-which(fit_23 == fit_23[4])]
# Only Select the lines where the parameters are.
fit_23 <- fit_23[(which(fit_23 == "MODEL RESULTS") + 2):(which(fit_23 == "RESULTS IN PROBABILITY SCALE") - 1)]

# Select rows where the items' parameters are (i.e., discrimination and severity of the
# two classes and the reparameterization of the severity)
fit_23 <- c(grep("REC", fit_23, value = TRUE), grep("BETA", fit_23, value = TRUE))

fit_23 <- data.frame(matrix(unlist(strsplit(fit_23, "\\s+")), ncol = 6, byrow = TRUE)[, -1])

# Turn string into numeric
fit_23[, 2:5] <- sapply(fit_23[, 2:5], function(x) as.numeric(levels(x))[x])

names(fit_23) <- c("Parameter", "Estimate", "S.E.", "Est/S.E.", "p-value")

# The data frame, fit_23, has five sets of parameters: The discrimination of Class 1,
# the thresholds of Class 1, the discrimination of Class 2, the thresholds of Class 2,
# and the severity (reparameterization of the thresholds) of Class 2. We are interested
# in the discrimination and the severity of Class 2.
I <- 23 # Number of items

alpha <- fit_23[(I * 2 + 1):(I * 3), ]
beta  <- fit_23[(I * 4 + 1):(I * 5), ]

# Plot discrimination and severity with their confidence intervals.

plot(1:I, alpha[, 2], ylim = c(0, 3.5), pch = 16, xaxt = "n", 
     ylab = "Discrimination", xlab = "Item")
segments(x0 = 1:I, 
         y0 = alpha[, 2] - 1.96 * alpha[, 3], 
         y1 = alpha[, 2] + 1.96 * alpha[, 3])
axis(1, 1:I, labels = alpha[, 1], cex.axis = 0.5, las = 2)

plot(1:I, beta[, 2], ylim = c(0, 12), pch = 16, xaxt = "n", 
     ylab = "Severity", xlab = "Item")
segments(x0 = 1:I, 
         y0 = beta[, 2] - 1.96 * beta[, 3], 
         y1 = beta[, 2] + 1.96 * beta[, 3])
axis(1, 1:I, labels = alpha[, 1], cex.axis = 0.5, las = 2)

# Clearly, this shows the worst items are C18, C27, c37, and C39.
# We repeated the analysis excluding these items.

fit_19 <- readLines("sidi19.out", skipNul = TRUE)
# Exclude empty lines.
fit_19 <- fit_19[-which(fit_19 == fit_19[4])]
# Only Select the lines where the parameters are.
fit_19 <- fit_19[(which(fit_19 == "MODEL RESULTS") + 2):(which(fit_19 == "RESULTS IN PROBABILITY SCALE") - 1)]

# Select rows where the items' parameters are (i.e., discrimination and severity of the
# two classes and the reparameterization of the severity)
fit_19 <- c(grep("REC", fit_19, value = TRUE), grep("BETA", fit_19, value = TRUE))

fit_19 <- data.frame(matrix(unlist(strsplit(fit_19, "\\s+")), ncol = 6, byrow = TRUE)[, -1])

# Turn string into numeric
fit_19[, 2:5] <- sapply(fit_19[, 2:5], function(x) as.numeric(levels(x))[x])

names(fit_19) <- c("Parameter", "Estimate", "S.E.", "Est/S.E.", "p-value")

# The data frame, fit_19, has five sets of parameters: The discrimination of Class 1,
# the thresholds of Class 1, the discrimination of Class 2, the thresholds of Class 2,
# and the severity (reparameterization of the thresholds) of Class 2. We are interested
# in the discrimination and the severity of Class 2.
I <- 19 # Number of items

alpha <- fit_19[(I * 2 + 1):(I * 3), ]
beta  <- fit_19[(I * 4 + 1):(I * 5), ]

# Plot discrimination and severity with their confidence intervals.

plot(1:I, alpha[, 2], ylim = c(0, 3.5), pch = 16, xaxt = "n", 
     ylab = "Discrimination", xlab = "Item")
segments(x0 = 1:I, 
         y0 = alpha[, 2] - 1.96 * alpha[, 3], 
         y1 = alpha[, 2] + 1.96 * alpha[, 3])
axis(1, 1:I, labels = alpha[, 1], cex.axis = 0.5, las = 2)

plot(1:I, beta[, 2], ylim = c(0, 6), pch = 16, xaxt = "n", 
     ylab = "Severity", xlab = "Item")
segments(x0 = 1:I, 
         y0 = beta[, 2] - 1.96 * beta[, 3], 
         y1 = beta[, 2] + 1.96 * beta[, 3])
axis(1, 1:I, labels = alpha[, 1], cex.axis = 0.5, las = 2)
