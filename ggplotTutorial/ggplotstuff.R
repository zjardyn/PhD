library(tidyverse)
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy))
pivot_longer(mpg,cty:hwy, names_to = "mpg_type", values_to = "mpg") %>%
  ggplot(aes(x=displ, y = mpg, shape = mpg_type, colour = mpg_type))+
  geom_point()

ggplot(mpg, aes(x=displ, y=cty, shape =trans ))+ 
  geom_point()

ggplot(mpg, aes(cty, trans, colour = trans)) + geom_point() + geom_jitter(width = .1)

ggplot(mpg, aes(trans, displ, colour = class)) + geom_point()

ggplot(mpg, aes(displ, class)) + geom_point()+ facet_wrap(~cyl)

ggplot(mpg, aes(hwy)) + geom_density()


ggplot(mpg, aes(fill = model, x = manufacturer)) + geom_bar()
ggplot(economics, aes(date, unemploy)) + geom_line()

ggplot(mpg, aes(displ, hwy)) + geom_point()+ facet_wrap(~cyl, nrow= 4, scales ="free")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(method = "lm")
#> `geom_smooth()` using formula 'y ~ x'

# assessing the distribution of categorical variables
pivot_longer(mpg,cty:hwy, names_to = "mpg_type", values_to = "mpg") %>%
  ggplot( aes(drv, mpg, size = cyl, colour = mpg_type)) + geom_jitter()
ggplot(mpg, aes(drv, hwy)) + geom_boxplot()
ggplot(mpg, aes(drv, hwy)) + geom_violin()

ggplot(mpg, aes(displ, colour = drv)) + 
  geom_freqpoly(binwidth = 0.5)
ggplot(mpg, aes(displ, fill = drv)) + 
  geom_histogram(binwidth = 0.5) + 
  facet_wrap(~drv, ncol = 1)

ggplot(mpg, aes(manufacturer)) + 
  geom_bar()

drugs <- data.frame(
  drug = c("a", "b", "c"),
  effect = c(4.2, 9.7, 6.1)
)

ggplot(drugs, aes(drug, effect)) + geom_bar(stat = "identity")
ggplot(drugs, aes(drug, effect)) + geom_point()
ggplot(mpg, aes(cty, hwy)) + geom_boxplot()


ggplot(mpg, aes(class, hwy)) + geom_boxplot()
ggplot(mpg, aes(reorder(class, hwy, decreasing = TRUE), hwy)) + geom_boxplot()

ggplot(diamonds, aes(carat)) + geom_histogram(bins = 500)
# ggplot(diamonds, aes(price)) + geom_histogram() + facet_wrap(~carat)

ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 3) +
  ggtitle("hello")


ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25)

ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25) + 
  xlim("f", "r") + 
  ylim(20, 30)
ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25, na.rm = TRUE) + 
  ylim(20, NA)


df <- data.frame(
  x = c(3, 1, 5), 
  y = c(2, 4, 6), 
  label = c("a","b","c")
)
p <- ggplot(df, aes(x, y, label = label)) + 
  labs(x = NULL, y = NULL) + # Hide axis label
  theme(plot.title = element_text(size = 12)) # Shrink plot title
p + geom_point() + ggtitle("point")
p + geom_text() + ggtitle("text")
p + geom_bar(stat = "identity") + ggtitle("bar")
p + geom_tile() + ggtitle("raster")

p + geom_line() + ggtitle("line")
p + geom_area() + ggtitle("area")
p + geom_path() + ggtitle("path")
p + geom_polygon() + ggtitle("polygon")

data(Oxboys, package = "nlme")
head(Oxboys)

ggplot(Oxboys, aes(age, height, group = Subject)) + 
  geom_point() + 
  geom_line()

ggplot(Oxboys, aes(age, height, group = Subject)) + 
  geom_line() + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(Oxboys, aes(age, height)) + 
  geom_line(aes(group = Subject)) + 
  geom_smooth(method = "lm", size = 2, se = FALSE)
#> `geom_smooth()` using formula 'y ~ x'
ggplot(Oxboys, aes(Occasion, height)) + 
  geom_boxplot() +
  geom_line(aes(group = Subject), colour = "#3366FF", alpha = 0.5)

ggplot(mpg, aes(class)) + 
  geom_bar()
ggplot(mpg, aes(class, fill = drv)) + 
  geom_bar(colour = "white")

ggplot(mpg, aes(class, fill = factor(hwy))) + 
  geom_bar()
ggplot(mpg, aes(class, fill = hwy, group = hwy)) + 
  geom_bar()


ggplot(mpg, aes(hwy)) + geom_boxplot()
ggplot(mpg, aes(hwy, manufacturer)) + geom_boxplot()

ggplot(mpg, aes(hwy, cyl, group = cyl)) + geom_boxplot()
ggplot(mpg, aes(hwy)) + geom_boxplot()+ facet_wrap(~cyl, nrow = 4)

ggplot(mpg, aes(displ, cty))+
  geom_boxplot(aes(group = cut_interval(cyl, 100)))

ggplot(mpg, aes(displ, cty, group = displ))+
  geom_boxplot()

df <- data.frame(x = 1:3, y = 1:3, colour = c(1,3,5))

ggplot(df, aes(x, y, colour = factor(colour))) + 
  geom_line(aes(group = 1), size = 2) +
  geom_point(size = 5)

ggplot(df, aes(x, y, colour = colour)) + 
  geom_line(aes(group = 1), size = 2) +
  geom_point(size = 5)


ggplot(mpg, aes(drv)) + 
  geom_bar(colour = "white")

ggplot(mpg, aes(drv, fill = hwy, group = hwy)) + 
  geom_bar(colour = "white")

library(dplyr)  
mpg2 <- mpg %>% arrange(hwy) %>% mutate(id = seq_along(hwy)) 
# View(mpg2)
ggplot(mpg2, aes(drv, fill = hwy, group = id)) + 
  geom_bar(colour = "white")

library(babynames)
hadley <- dplyr::filter(babynames, name == "Hadley")
ggplot(hadley, aes(year, n, group = sex, colour = sex)) + 
  geom_line()


y <- c(18, 11, 16)
df <- data.frame(x = 1:3, y = y, se = c(1.2, 0.5, 1.0))

base <- ggplot(df, aes(x, y, ymin = y - se, ymax = y + se))
base + geom_crossbar()
base + geom_pointrange()
base + geom_smooth(stat = "identity")

base + geom_errorbar()
base + geom_linerange()
base + geom_ribbon()

# Unweighted
ggplot(midwest, aes(percwhite, percbelowpoverty)) + 
  geom_point()

# Weight by population
ggplot(midwest, aes(percwhite, percbelowpoverty)) + 
  geom_point(aes(size = poptotal / 1e6)) + 
  scale_size_area("Population\n(millions)", breaks = c(0.5, 1, 2, 4))

#Unweightedt
ggplot(midwest, aes(percwhite, percbelowpoverty)) + 
  geom_point() + 
  geom_smooth(method = lm, size = 1)
#> `geom_smooth()` using formula 'y ~ x'

# Weighted by population
ggplot(midwest, aes(percwhite, percbelowpoverty)) + 
  geom_point(aes(size = poptotal / 1e6)) + 
  geom_smooth(aes(weight = poptotal), method = lm, size = 1) +
  scale_size_area(guide = "none")
#> `geom_smooth()` using formula 'y ~ x'

ggplot(midwest, aes(percbelowpoverty)) +
  geom_histogram(binwidth = 1) + 
  ylab("Counties")

ggplot(midwest, aes(percbelowpoverty)) +
  geom_histogram(aes(weight = poptotal), binwidth = 1) +
  ylab("Population (1000s)")

ggplot(diamonds, aes(depth)) + 
  geom_histogram()
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
ggplot(diamonds, aes(depth)) + 
  geom_histogram(binwidth = 0.1) + 
  xlim(55, 70)
#> Warning: Removed 45 rows containing non-finite values (stat_bin).
#> Warning: Removed 2 rows containing missing values (geom_bar).


ggplot(diamonds, aes(depth)) + 
  geom_freqpoly(aes(colour = cut), binwidth = 0.1, na.rm = TRUE) +
  xlim(58, 68) 

ggplot(diamonds, aes(depth)) + 
  geom_histogram(aes(fill = cut), binwidth = 0.1, position = "fill",
                 na.rm = TRUE) +
  xlim(58, 68) + 
  theme(legend.position = "none") 
# density
ggplot(diamonds, aes(depth)) +
  geom_density(na.rm = TRUE) + 
  xlim(58, 68) + 
  theme(legend.position = "none")
ggplot(diamonds, aes(depth, fill = cut, colour = cut)) +
  geom_density(alpha = 0.2, na.rm = TRUE, adjust = 1/4) + 
  xlim(58, 68) + 
  theme(legend.position = "none")

ggplot(diamonds, aes(clarity, depth)) + 
  geom_boxplot()
ggplot(diamonds, aes(carat, depth)) + 
  geom_boxplot(aes(group = cut_width(carat, .1))) + 
  xlim(NA, 2.05)

ggplot(diamonds, aes(clarity, depth)) + 
  geom_violin()
# total/number you want
ggplot(diamonds, aes(carat, depth)) + 
  geom_violin(aes(group = cut_width(carat, 2/5))) + 
  xlim(NA, 2.05)

ggplot(mpg, aes(displ, cty)) + 
  geom_boxplot(aes(group = cut_width(displ, 7/4)))

ggplot(diamonds,aes(carat)) + 
  geom_histogram(binwidth = 0.1)
