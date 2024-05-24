#load library
library(fpp2)
library(patchwork)
library(ggstatsplot)
library(ggthemes)
library(tidyverse)

#sin(x)
sin(ts(seq(from = 1, to = 100, by = 0.001))) %>%
  naive() %>%
  autoplot()
 
#mape function
MAPE <- function(x) {
  abs((x-lag(x))/lag(x))*100
}

#Computing Mean
sin(seq(from = 1, to = 100, by = 0.01)) %>%
  MAPE() %>%
  as.ts() %>%
  as.data.frame() -> mean_compute

mean_compute[-1,] %>%
  mean()

gghistostats(mean_compute, x)+
  labs(title = "Distribution of Absolute % Change Values over which the Mean for MAPE is Hypothetically Computed",
       subtitle = "The histogram is extremely biased; The mean of 5.9 is non-representive.",
       caption = "") -> meandist

print(sqrt(count(mean_compute)))

#plots
sin(seq(from = 1, to = 100, by = 0.01)) %>%
  MAPE() %>%
  as.ts() %>%
  autoplot() + theme_tufte() + labs(
    title = "Absolute % Change",
    subtitle = "Visualizes the percentage change in the above sin function. As the denomoniator approaches 0, the percentage change values are greater.\nAs such the mean is heavily biased. ",
    caption = "graph by @abgaoadam"
  ) -> fmape


sin(ts(seq(from = 1, to = 100, by = 0.01))) %>%
  naive() %>%
  autoplot()+ theme_tufte() + labs(
    title = "Sin Function", 
    subtitle = "Function is defined by sin(x), x = 1:100 by .0."
  ) -> fsin

fsin /
  fmape /
  meandist







