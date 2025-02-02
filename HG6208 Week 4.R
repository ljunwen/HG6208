# sets up a dataframe with the x- and y-values according to the standard normal distribution using the function 'dnorm'

x <- seq(from = -5, to = 5, by = 0.01)
dist <- as.data.frame(x)
dist$y <- dnorm(x, mean = 0, sd = 1)


# plots the graph using the x- and y-values in the dataframe

plot(dist$x,dist$y, lty = 1, type = "l", ylim = c(0,1), col = "blue")


# changing the mean and standard deviation of the distribution

mean <- 1
stdev <- 2
dist$y <- dnorm(x, mean = mean, sd = stdev)
lines(dist$x,dist$y, lty = 2, col = "blue")


# transforming the distribution back to the standard normal distribution

dist$x <- (dist$x - 1)   # moving the distribution along the x-axis
lines(dist$x,dist$y, lty = 2, lwd = 2, col = "red")

dist$x <- dist$x/2       # expanding or contracting the distribution on both sides of the mean
dist$y <- dist$y*2       # keeping the area under the curve constant after the transformation
lines(dist$x,dist$y, lty = 2, lwd = 2, col = "red")


# downloads the data file from the course's GitHub
path <- ""
download.file("https://github.com/ljunwen/HG6208/raw/main/data/Week%202%20-%20RT%20data.txt", paste0(path, "Week 2 - RT data.txt"), method = "libcurl")
download.file("https://github.com/ljunwen/HG6208/raw/main/data/Week%202%20-%20Heights.txt", paste0(path, "Week 2 - Heights.txt"), method = "libcurl")

# simulation of distribution of sample means

mean <- 0
stdev <- 2

# create a dataset of 1000 numbers from a normal distribution of mean 'mean' and standard deviation 'stdev'
x <- rnorm(1000, mean, stdev)
dist <- as.data.frame(x)

# plot a histogram of the dataset
hist(dist$x)

# create a dataset of sample means with sample size 'n' and the number of samples 'num_samples'
n <- 5
num_samples <- 5000

samples <- data.frame(matrix(vector(), num_samples, 1, dimnames=list(c(), "mean")))

for (i in seq_along(1:(num_samples))) {
   samples$mean[i] <- mean(sample(dist$x, n))  
}

hist(samples$mean)

# calculates the actual standard deviation of the sample means as well as the calculated standard deviation from the population standard deviation
sd(samples$mean)
stdev/sqrt(n)

# sampling using the heights data

heights <- read.csv(paste0(path, "Week 2 - Heights.txt"), header = TRUE, stringsAsFactors = TRUE)
hist(heights$Height)

n <- 5
num_samples <- 5000

samples <- data.frame(matrix(vector(), num_samples, 1, dimnames=list(c(), "mean")))

for (i in seq_along(1:(num_samples))) {
  samples$mean[i] <- mean(sample(heights$Height, n))  
}

hist(samples$mean)


# t-tests

Data <- read.delim(file = paste0(path, "Week 2 - RT data.txt"), header = TRUE, stringsAsFactors = TRUE)
levels(Data$Drug)
levels(Data$Participant)

t.test(RT ~ Drug, data = Data)
t.test(Pair(RT[Drug == "Yes"], RT[Drug == "No"]) ~ 1, data = Data[order(Data$Participant),])

# non-parametric tests
wilcox.test(RT ~ Drug, data = Data)
wilcox.test(Pair(RT[Drug == "Yes"], RT[Drug == "No"]) ~ 1, data = Data[order(Data$Participant),])
