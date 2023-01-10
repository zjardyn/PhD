library(traitdataform)
library(tidyverse)

pulldata("pantheria")
df <- pantheria
vars <- colnames(df)

vars <- str_remove(vars, "^[:alnum:]+_") %>%
  str_remove("^[:alnum:]+\\.[:digit:]_")

colnames(df) <- vars

ggplot(df, aes(LitterSize)) +
  geom_histogram()

ggplot(df, aes(AdultBodyMass_g, AdultHeadBodyLen_mm))+
  geom_point()

ggplot(data = df) +
  geom_histogram(mapping = aes(x = AdultBodyMass_g)) +
  labs(title = "Histogram of Mammal Body Mass", x = "Adult Body Mass (g)", y = "Count")

ggplot(data = df) +
  geom_boxplot(mapping = aes(x = Order, y = AdultBodyMass_g), outlier.color = "red", outlier.fill = "red", outlier.size = 2) +
  labs(title = "Boxplot of Mammal Body Mass", x = "Mammal Orders", y = "Adult Body Mass (g)") +
  coord_flip()

df1.xCetacea <- df %>%
  filter(!Order == "Cetacea")

ggplot(data = df1.xCetacea) +
  geom_boxplot(mapping = aes(x = Order, y = AdultBodyMass_g), outlier.color = "red", outlier.fill = "red", outlier.size = 2) +
  labs(title = "Boxplot of Mammal Body Mass", x = "Mammal Orders", y = "Adult Body Mass (g)") +
  coord_flip() +
  scale_x_discrete(limits = c(sort(x = unique(df1.xCetacea$Order), decreasing = T)))

df1 <- df
ggplot(data = df1) +
  geom_histogram(mapping = aes(x = log10(AdultBodyMass_g))) +
  labs(title = "Histogram of Mammal Body Mass", x = "Log10 of Adult Body Mass (g)", y = "Count")

#OK. Now looking at maximum longevity
ggplot(data = df1) +
  geom_histogram(mapping = aes(x = MaxLongevity_m)) +
  labs(title = "Histogram of Longevity in Mammals", x = "Maximum Longevity (m)", y = "Count")

#Looking at ln-transformed longevity
ggplot(data = df1) +
  geom_histogram(mapping = aes(x = log(MaxLongevity_m))) +
  labs(title = "Histogram of Longevity in Mammals", x = "(ln) Maximum Longevity (m)", y = "Count")

#Now, plotting body mass vs. longevity as a scatterplot
ggplot(data = df1) +
  geom_point(mapping = aes(x = log10(AdultBodyMass_g), y = log(MaxLongevity_m))) +
  labs(title = "Body Size vs. Longevity in Mammals", x = "(log10) Adult Body Size (g)", y = "(ln) Maximum Longevity (m)")


ggplot(data = df1) +
  geom_point(mapping = aes(x = log10(AdultBodyMass_g), y = log(MaxLongevity_m), colour = Order)) +
  labs(title = "Body Size vs. Longevity in Mammals", x = "(log10) Adult Body Size (g)", y = "(ln) Maximum Longevity (m)")

df1.large.orders <- df1 %>%
  group_by(Order) %>%
  filter(n() >= 200)

#repeating our scatterplot for orders containing >= 200 species
ggplot(data = df1.large.orders) +
  geom_point(mapping = aes(x = log10(AdultBodyMass_g), y = log(MaxLongevity_m), colour = Order)) +
  labs(title = "Body Size vs. Longevity in Species-Rich Mammal Orders", x = "(log10) Adult Body Size (g)", y = "(ln) Maximum Longevity (m)")

#Repeating our scatterplot for orders containing >= 200 species, this time using both colour and symbol shape to show the orders. We can use colour and symbols with redundancy to make plots more accessibe.
ggplot(data = df1.large.orders) +
  geom_point(mapping = aes(x = log10(AdultBodyMass_g), y = log(MaxLongevity_m), colour = Order, shape = Order)) +
  labs(title = "Body Size vs. Longevity in Species-Rich Mammal Orders", x = "Log10 Adult Body Size (g)", y = "Ln Maximum Longevity (m)")

