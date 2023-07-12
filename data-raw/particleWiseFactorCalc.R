
# For a particlewise processing, the blank correction has to be modified
# Therefore, particles will only be corrected if they have a high similarity. 
# Then the most similar particle will be substracted

# Here we calculate a factor that determines how big the size difference can be.
# In this, the factor should be smaller for bigger particles.
# For calculating the factor, we define the range that seems appropriate for the
# smallest (5.5 µm, 24 µm2) and biggest (500 µm, 196000 µm2) particles and calculate a linear relation 
# along this size range.

# range to check how it is distributed
range <- data.frame(length = seq(from = 1, to = 600, length.out = 200),
                    area = NA,
                    factor = NA)
range$area <- ((range$length/2)^2)*pi

# preliminary definition for the two extremes (all other values should be calculated accordingly)
def <- data.frame(length = c(5.5, 101.402010, 500),
                  area = c(23.75829, 8075.753, 196349.54),
                  factor = c(1, 0.5, 0.1))

# linear calculation
eq <- lm(factor ~ area, data = def)
summary(eq)

range$factor <- predict(eq, newdata = range)

library(ggplot2)
ggplot(data = range, aes(y = factor, x = area))+
  geom_line()
ggplot(data = range, aes(y = factor, x = length))+
  geom_line()

# -> this might become problematic with particles above 740 µm (diameter)

# non-linear calculation
eq <- nls(factor ~ 1*a^(area) + 0.1, data = def, start = list(a = 1))
summary(eq)
# or
eq <- nls(factor ~ 1*area^-a+0.1, data = def, start = list(a = 1))
summary(eq)

range$factor <- predict(eq, newdata = range)

ggplot(data = range, aes(y = factor, x = area))+
  geom_line()
ggplot(data = range, aes(y = factor, x = length))+
  geom_line()

# check how much it is
range$coverageArea <- range$area * range$factor
range$coverageLength <- range$length * range$factor

# -> factor ~ 1*a^area + 0.1 with a = 0.999 seems to be quite decent
range$factor <- 1*0.9999^range$area + 0.1



######## other Idea ######
# creating a histogram-like model that predicts the approximate number of particles of different sizes. 
# Then using the integral under the curve within a certain range (e.g., a 10th of the whole range) and 
# substracting a number of particles derived from that integral that are closest to those of the blank in that range
# if no particle in that range could be found in the blank, a random particle will be deleted
# the model gets more precise with more blanks, satisfying the effort of taking more blanks.
# That way, the dilemma of how to deal with several blank replicates is solved.

dist.data <- data.frame(polymer = "PP",
                        size = c(5, 5.8, 12, 50, 100, 106, 458))

library(mgcv)
test.model <- gam(size ~ 1, data = dist.data)
summary(test.model)

predict(test.model, newdata = dist.data)

hist.test <- hist(dist.data$size)

library(ggplot2)

dingsdata <- data.frame(density = hist.test$density,
                        breaks = hist.test$breaks[-1])
ggplot(data = dingsdata, aes(y = density, x = breaks))+
  geom_line()

# the question remains, which particle to use from the blank to substract from the sample.
# however, with this approach, the size range of which particles can be used, could be defined by
# the size range, where the integral to becomes 1

# then take integral:
# https://stackoverflow.com/questions/4954507/calculate-the-area-under-a-curve