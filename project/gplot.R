# Install the ggplot package (don't need to do this on lab machines)
# (if you scroll down to the bottom of the list there's a mirror in
# Iowa that should be fast and reliable)
install.packages("ggplot2")

# Load the ggplot package
# - do this every time you want to use the ggplot package
library(ggplot)

# Examining distributions
# ======================================================

# Histograms ---------------------------------------
qplot(price, data=diamonds, geom="histogram")

# ALWAYS EXPERIMENT WITH THE BIN SIZE!
qplot(price, data=diamonds, geom="histogram", binwidth=500)
# ALWAYS EXPERIMENT WITH THE BIN SIZE!
qplot(price, data=diamonds, geom="histogram", binwidth=100)
# ALWAYS EXPERIMENT WITH THE BIN SIZE!
qplot(price, data=diamonds, geom="histogram", binwidth=50)

# Investigating relationships
# ======================================================

# Two continuous variables -----------------------------------
# Use a scatterplot
qplot(price, carat, data=diamonds)
qplot(log(carat), log(price), data=diamonds)
qplot(carat, price/carat, data=diamonds)

# Map extra variables to other aesthetic attributes
qplot(carat, price, data=diamonds, colour=color)
qplot(carat, price, data=diamonds, size=carat)
qplot(carat, price, data=diamonds, shape=cut)

# Facetting displays the same plot for different subsets of the data
# use facets argument, rows on left hand-size 
qplot(price, carat, data=diamonds, facets = . ~ color)
qplot(price, carat, data=diamonds, facets = color ~ clarity)

# One continuous, one categorical -----------------------------------

# There is too much overplotting in a scatterplot to be very useful
# we can basically only see the range of the data
qplot(color, price/carat, data=diamonds)

# We could spread the points out a little bit, by adding random jitter
# This helps a little bit, but it's still hard to see what's going on
qplot(color, price/carat, data=diamonds, position="jitter")
qplot(color, price/carat, data=diamonds, position=position_jitter(x=2))

# Another technique is to visualise the conditional distribution, ie.
# for a given colour, what is the distribution of price/colour.
# A boxplot provides a simple summary of the distribution:
qplot(color, price/carat, data=d, geom="boxplot")

# Or we can use a histogram for each colour to look at the shape of
# the distribution in more detail:
qplot(price/carat, data=d, facet= color ~ ., geom="histogram")

# ALWAYS EXPERIMENT WITH THE BIN SIZE!
qplot(price/carat, data=d, facet= color ~ ., geom="histogram", binwidth=100)

# Two categorical variables -----------------------------------
# Use a fluctuation diagram - a visualisation of the contingency table

ggfluctuation(table(d$cut, d$color))

# Zooming ---------------------------------------
# You can zoom in on an interesting area of the plot using the 
# xlim and ylim arguments:

qplot(price, data=d, geom="histogram", xlim=c(0, 5000))
