#Installing and loading the agridat package
install.packages("agridat")
library(agridat)

sessionInfo()
devtools::session_info()

#loading the nass.corn dataset
data <- nass.corn
data1 = cramer.cucumber

#intall and load dplyr
install.packages("dplyr")
library(dplyr)

#ctrl + shift + m
data_year <- data %>%
  filter(year %in% c("1866", "1867", "1868"))

data_state <- data %>%
  filter(state %in% c("Alabama", "Arkansas", "Georgia", "California"))

data_year_state <- data %>%
  filter(state %in% c("Alabama", "Arkansas", "Georgia", "California") & year %in% c("1866", "1867", "1868"))

#explore acres
range(data$acres)
hist(data$acres)

data_acres <- data %>%
  filter(acres >= 2000000, acres <= 6000000)


#Install and load summarytools
install.packages("summarytools")
library(summarytools)

dfSummary(data)
view(dfSummary(data, style = "grid", graph.magnif = 0.82, varnumbers = FALSE))
#file = "data_summary.html", report.title = "Data summary"


#drop the unused levels
data_state <- droplevels(data_state)

#max.distinct.values = 50
#valid.col = F
#graph.col = F
#na.col = 
#class = T
#graph.col =

descr(data)
descr(data,
      stats     = c("mean", "sd"),
      transpose = TRUE, #See what false does
      headings  = FALSE)

bystate <- with(data_state, stby(data   = data_state,
                INDICES = state,
                FUN     = descr,
                stats   = "common",
                transpose = FALSE))
view(bystate, file   = "descr_by_state.html") #saving the file


#Same but with psych - easy for on the go
install.packages("psych")
library(psych)

describe(data)
describe(data, skew=FALSE,ranges=FALSE)
t(describe(data, skew=FALSE,ranges=FALSE))

describeBy(data, data$state)
describeBy(data, data$state, skew=FALSE,ranges=FALSE)
output <- describeBy(data, data$state, skew=FALSE,ranges=FALSE)
output_t <- lapply(output, function(x) as.data.frame(t(x)))

#Saving it
output <- describeBy(data, data$state, skew=FALSE,ranges=FALSE, mat = TRUE)
write.csv(output, file = "output.csv", row.names = T)

#GGally
install.packages("GGally")
library(GGally)
a
ggpairs(data)
ggpairs(data, cardinality_threshold = 48)

#diag = list(continuous = wrap("barDiag", bins = 15, fill = "blue")

#switch to data_state
ggpairs(data_state)
data %>%
  filter(state %in% c("Alabama", "Arkansas", "Georgia", "California")) %>%
  ggpairs()

#drop levels

data %>%
  filter(state %in% c("Alabama", "Arkansas", "Georgia", "California")) %>%
  droplevels() %>%
  ggpairs()

#add colour
data %>%
  filter(state %in% c("Alabama", "Arkansas", "Georgia", "California")) %>%
  droplevels() %>%
  ggpairs(aes(color = state))

#Change the theme (https://ggplot2-book.org/themes.html)
data %>%
  filter(state %in% c("Alabama", "Arkansas", "Georgia", "California")) %>%
  droplevels() %>%
  ggpairs(aes(color = state)) + theme_bw()

#add alpha
data %>%
  filter(state %in% c("Alabama", "Arkansas", "Georgia", "California")) %>%
  droplevels() %>%
  ggpairs(aes(color = state, alpha = 0.5)) + theme_bw()

#show labels
data %>%
  filter(state %in% c("Alabama", "Arkansas", "Georgia", "California")) %>%
  droplevels() %>%
  ggpairs(aes(color = state, alpha = 0.5), showStrips = TRUE) + theme_bw()

#spearman for non-normal data
data %>%
  filter(state %in% c("Alabama", "Arkansas", "Georgia", "California")) %>%
  droplevels() %>%
  ggpairs(aes(colour = state), 
          upper = list(continuous = wrap("cor", method = "pearson")))

#modify colours
data %>%
  filter(state %in% c("Alabama", "Arkansas", "Georgia", "California")) %>%
  droplevels() %>%
  ggpairs(aes(color = state, alpha = 0.5), showStrips = TRUE) + 
  theme_bw() +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "pink")) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "pink"))

install.packages("ggthemes")
library(ggthemes) # https://r-charts.com/ggplot2/themes/

data %>%
  filter(state %in% c("Alabama", "Arkansas", "Georgia", "California")) %>%
  droplevels() %>%
  ggpairs(aes(color = state, alpha = 0.5), showStrips = TRUE) + 
  theme_economist() +
  scale_fill_economist()


#selecting custom plots
plots <- data %>%
  filter(state %in% c("Alabama", "Arkansas", "Georgia", "California")) %>%
  droplevels() %>%
  ggpairs(aes(color = state, alpha = 0.5), showStrips = TRUE) + 
  theme_economist() +
  scale_fill_economist()

plots[3,1] + geom_smooth(method = "loess", span = 0.1)

plots[3,2]
a <- plots[3,2]
a + theme(legend.position = "none")
a + guides(alpha = "none") + 
    labs(fill = "State",
         y = "State",
         x = "Acres",
         title = "New plot title", 
         subtitle = "A subtitle",
         caption = "(based on data from ...)",
         tag = "A") + 
    theme(axis.title.y = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
          axis.text.y = element_text(angle = 45, hjust = 1, face = "italic"),
          legend.position = "right",
          legend.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 16, face = "bold", hjust=0 )
          #legend.position = "none",
          #panel.grid.major = element_blank()
          )

#https://ggplot2.tidyverse.org/reference/labs.html - for labs
#https://ggplot2.tidyverse.org/reference/theme.html - for theme

#Correlations
library(easystats)
# data <- mtcars
result <- correlation(data) #method = "spearman" / "pearson" - spearman for non-normal
summary(result)

#check for normality
ks.test(data$acres, "pnorm", mean=mean(data$acres), sd=sd(data$acres))
lapply(data1[, 1:6], function(x) ks.test(x, "pnorm", mean(x), sd(x)))

#or
shapiro.test(data$acres)
lapply(data1[, 1:6], shapiro.test)

set.seed(123)
sample_acres <- sample(data$acres, 5000)
shapiro.test(sample_acres)

qqnorm(data$yield)
qqline(data$yield)

#filter in line
data %>%
  filter(year >= 1986, year <= 1995 & state %in% c("Alabama", "Arkansas", "Georgia", "California")) %>%
  droplevels() %>%
  dplyr::select(acres, yield, state) %>%
  group_by(state) %>%
  correlation(method = "spearman") %>% 
  summary()

#plot corr and partial
data %>%
  correlation(partial = TRUE) %>%
  summary()

plot(correlation(data, partial = TRUE)) +
  scale_edge_color_continuous(low = "#000004FF", high = "#FCFDBFFF")

library(ggstatsplot)
a = ggcorrmat(data, type = "non-parametric", label = TRUE)

a +
  scale_x_discrete(labels = c(
    rep = "Replicate",
    plants  = "Number of plants",
    cycle  = "Experimental cycle",
    flowers   = "Number of flowers"
  )) +
  scale_y_discrete(labels = c(
    rep = "Replicate",
    plants  = "Number of plants",
    cycle  = "Experimental cycle",
    flowers   = "Number of flowers"
  )) +
  theme(legend.position = "none")

a <- grouped_ggcorrmat(
       data = data1,
       cor.vars = c(flowers, branches, leaves, totalfruit, earlyfruit),
       grouping.var = cycle, type = "non-parametric")

a[[1]]

#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
library(corrplot)
M <- cor(data, method = "spearman")  # or "pearson"

data12 <- data1 %>% dplyr::select(flowers, branches, leaves, totalfruit, earlyfruit)

corrplot(M, method = "circle")       # methods: "ellipse", "number", "shade", "pie", "color"
corrplot(M, method = 'shade', order = 'AOE', diag = F, type = 'lower')
corrplot.mixed(M, order = 'AOE')
corrplot.mixed(M, lower = 'shade', upper = 'circle', order = 'hclust')
corrplot(M, method = 'shade', order = 'hclust', diag = F)
corrplot(M, order = "alphabet", col = c("white", "black"), bg = "gold2", tl.col = "gold2")

testRes = cor.mtest(data, conf.level = 0.95)
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)

#partial
library(ppcor)
M <- pcor(data, method = "spearman")
M = M$estimate
#same drill from here

pcor.test(x = data1$totalfruit / data1$plants,
          y = data1$totalfruit,
          z = data1[, c("cycle", "rep")],
          method = "spearman")

detach("package:MASS", unload = TRUE)


# nass.barley
# nass.corn
# nass.cotton
# nass.hay
# nass.sorghum
# nass.wheat
# nass.rice
# nass.soybean