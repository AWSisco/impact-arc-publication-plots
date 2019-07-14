# Name:         before_after_bars.R
# Author:       Adam W. Sisco
# Last updated: 12 Jul 2019
# Description:  Plots the number of findings by priority for 3 DAACs: GHRC, ORNL, and SEDAC

#================================================

library(ggplot2)
library(scales)
library(magrittr)
library(cowplot)

#================================================

#Data from the "BEDI Before/After Metrics (by Adam Sisco)" Google Sheet
#GHRC+ORNL+SEDAC
high_before <- 5186+279+539
medium_before <- 1431+127+640
low_before <- 76+52+389 

high_after <- 707+14+13
medium_after <- 73+8+15
low_after <- 78+40+2
  
priority <- c("High","Medium","Low", "High", "Medium", "Low")
issues <- c(high_before, medium_before, low_before, high_after, medium_after, low_after)
when <- c("Before", "Before", "Before", "After", "After", "After")

df <- data.frame(priority, issues, when)
df$when <- factor(df$when, levels = levels(df$when)[2:1])

p <- ggplot(df, aes(x = priority,
                    y = issues,
                    fill = priority
                    )) +
  geom_bar(stat = "identity",
           width = 0.7,
           alpha = 0.8) +
  theme_minimal_hgrid() +
  scale_x_discrete(limits = c("High", "Medium", "Low")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("High" = rgb(224,102,102,maxColorValue = 255),
                               "Medium" = rgb(255, 239, 0,maxColorValue = 255),
                               "Low" = rgb(74,134,232,maxColorValue = 255))) +
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(0,10,0,0)))+
  labs(x = "Priority",
       y = "Number of Findings")

change <- c(((df$issues[4] - df$issues[1])/df$issues[1]),
            ((df$issues[5] - df$issues[2])/df$issues[2]),
            ((df$issues[6] - df$issues[3])/df$issues[3]))

change %>% round(digits = 3) %>% percent() -> change
ann_text <- data.frame(priority = c("High","Medium","Low"), issues = df$issues[4:6] + 125, lab = change,
                       when = factor("After",levels = c("Before","After")))

p + geom_text(data = ann_text, label = change) + facet_grid(cols = vars(when)) +
  theme(panel.spacing = unit(2, "lines"))

ggsave(filename = "before_after_bars.png", plot = last_plot(), dpi = 300,  width = 10, height = 6, units = "in")
